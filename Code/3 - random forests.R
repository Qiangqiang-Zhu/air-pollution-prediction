#### R script for Random Forests

# Helper packages
library(tidyverse)
library(doParallel)  # for parallel backend to foreach
library(foreach)     # for parallel processing with for loops
library(ggsci)
library(ggpubr)
library(data.table)

# Modelling packages
library(caret)       # for general model fitting
library(rpart)       # for fitting decision trees
library(ipred)       # for fitting bagged decision trees
library(ranger)      # a C++ implementation of random forest
library(rfinterval)

source("./Code/rflog.R")

nCl <- 32  # number of clusters

## Data preparation ------------------------------------------------------------

population <- fread("../Data/Site Population Density.csv")[, Type := NULL]
roads <- fread("../Data/Monitoring Sites Roads.csv") %>%
  .[, `:=`(Type = NULL, log_nearest_road_distance = log(nearest_road_distance))] %>%
  mutate(across(where(is.numeric), ~ . / 1000))

data_no2 <- fread("../Data/Data - NO2.csv") %>%
  left_join(population, by = c("Site", "Year")) %>%
  left_join(roads, by = c("Site")) %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas")),
    logNO2_modelled = log(NO2_modelled)
  ) %>%
  arrange(Site, Year_Month)

data_pm10 <- fread("../Data/Data - PM10.csv") %>%
  left_join(population, by = c("Site", "Year")) %>%
  left_join(roads, by = c("Site")) %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas")),
    logPM10_modelled = log(PM10_modelled)
  ) %>%
  arrange(Site, Year_Month)

data_pm25 <- fread("../Data/Data - PM25.csv") %>%
  left_join(population, by = c("Site", "Year")) %>%
  left_join(roads, by = c("Site")) %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas")),
    logPM25_modelled = log(PM25_modelled)
  ) %>%
  arrange(Site, Year_Month)

## Root mean squared error
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

## Median absolute error
mae <- function(y, yhat) median(abs(y - yhat), na.rm = TRUE)

## Bias
bias <- function(y, yhat) mean(yhat - y)

## Coverage probability
coverage <- function(y, lwr, upr) mean(y >= lwr & y <= upr)

## Average interval width
aiw <- function(lwr, upr) mean(abs(upr - lwr))

## Prediction with intervals ---------------------------------------------------

rf_avg <- function(p = "NO2", num.trees, mtry, min.bucket, max.depth) {
  cl <- makeCluster(nCl)
  registerDoParallel(cl)

  hyper_grid_rf <- expand.grid(
    Split = paste0("Split", 1:10),
    num.trees = num.trees,
    mtry = mtry,
    min.bucket = min.bucket,
    max.depth = max.depth
  )

  data <- get(paste0("data_", str_to_lower(p))) %>%
    rename(y = paste0(p)) %>%
    filter(!is.na(y))

  # execute full cartesian grid
  pred_df <- foreach(
    i = 1:nrow(hyper_grid_rf),
    .packages = c("tidyverse", "ranger"),
    .export = c("rmse", "mae", "bias", "rfinterval"),
    .combine = rbind
  ) %dopar% {
    split_str <- as.character(hyper_grid_rf$Split[i])
    data_train <- data %>% filter(get(split_str) != "test")
    data_test <- data %>% filter(get(split_str) == "test")

    rf_fit <- rfinterval(
      formula = as.formula(
        paste(
          "y",
          paste("Type", "UR", "Year", "Month", paste0(p, "_modelled"),
                "Hurs", "Psl", "SfcWind", "Sun", "Rainfall", "Tas", "NDVI",
                "log_nearest_road_distance",
                sep = " + "),
          sep = " ~ "
        )
      ),
      train_data = data_train,
      test_data = data_test,
      method = "quantreg",
      alpha = 0.05,
      symmetry = TRUE,
      params_ranger = list(
        num.trees = hyper_grid_rf$num.trees[i],
        mtry = hyper_grid_rf$mtry[i],
        min.bucket = hyper_grid_rf$min.bucket[i],
        max.depth = hyper_grid_rf$max.depth[i],
        verbose = FALSE,
        seed = 123,
        respect.unordered.factors = "order"
      )
    )

    data_test %>%
      rename(Obs = y) %>%
      dplyr::select(Year_Month, Site, Obs) %>%
      mutate(Pred = rf_fit$testPred,
             Lower = rf_fit$quantreg_interval$lo,
             Upper = rf_fit$quantreg_interval$up,
             Split = i)
  }

  cv_df <- pred_df %>%
    dplyr::select(Obs, Pred, Lower, Upper, Split) %>%
    group_by(Split) %>%
    summarise(RMSE = rmse(Obs, Pred),
              MAE = mae(Obs, Pred),
              Bias = bias(Obs, Pred),
              Coverage = coverage(Obs, Lower, Upper),
              AIW = aiw(Lower, Upper))

  return(list(
    CV = apply(cv_df %>% select(-Split), 2, mean),
    CV_Split = cv_df,
    Pred = pred_df
  ))

  stopCluster(cl)
}

rf_sp_avg <- function(p = "NO2", num.trees, mtry, min.bucket, max.depth) {
  cl <- makeCluster(nCl)
  registerDoParallel(cl)

  hyper_grid_rf <- expand.grid(
    Split = paste0("Split", 1:10),
    num.trees = num.trees,
    mtry = mtry,
    min.bucket = min.bucket,
    max.depth = max.depth
  )

  data <- get(paste0("data_", str_to_lower(p))) %>%
    rename(y = paste0(p)) %>%
    filter(!is.na(y))

  # execute full cartesian grid
  pred_df <- foreach(
    i = 1:nrow(hyper_grid_rf),
    .packages = c("tidyverse", "ranger"),
    .export = c("rmse", "mae", "bias", "rfinterval"),
    .combine = rbind
  ) %dopar% {
    split_str <- as.character(hyper_grid_rf$Split[i])
    data_train <- data %>% filter(get(split_str) != "test")
    data_test <- data %>% filter(get(split_str) == "test")

    rf_fit <- rfinterval(
      formula = as.formula(
        paste(
          "y",
          paste("Type", "UR", "Year", "Month", paste0(p, "_modelled"), "Longitude",  "Latitude",
                "Hurs", "Psl", "SfcWind", "Sun", "Rainfall", "Tas", "NDVI",
                "log_nearest_road_distance",
                sep = " + "),
          sep = " ~ "
        )
      ),
      train_data = data_train,
      test_data = data_test,
      method = "quantreg",
      alpha = 0.05,
      symmetry = TRUE,
      params_ranger = list(
        num.trees = hyper_grid_rf$num.trees[i],
        mtry = hyper_grid_rf$mtry[i],
        min.bucket = hyper_grid_rf$min.bucket[i],
        max.depth = hyper_grid_rf$max.depth[i],
        verbose = FALSE,
        seed = 123,
        respect.unordered.factors = "order"
      )
    )

    data_test %>%
      rename(Obs = y) %>%
      dplyr::select(Year_Month, Site, Obs) %>%
      mutate(Pred = rf_fit$testPred,
             Lower = rf_fit$quantreg_interval$lo,
             Upper = rf_fit$quantreg_interval$up,
             Split = i)
  }

  cv_df <- pred_df %>%
    dplyr::select(Obs, Pred, Lower, Upper, Split) %>%
    group_by(Split) %>%
    summarise(RMSE = rmse(Obs, Pred),
              MAE = mae(Obs, Pred),
              Bias = bias(Obs, Pred),
              Coverage = coverage(Obs, Lower, Upper),
              AIW = aiw(Lower, Upper))

  return(list(
    CV = apply(cv_df %>% select(-Split), 2, mean),
    CV_Split = cv_df,
    Pred = pred_df
  ))

  stopCluster(cl)
}

rf_oc_no2 <- rf_avg("NO2", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 10)
rf_oc_pm10 <- rf_avg("PM10", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 15)
rf_oc_pm25 <- rf_avg("PM25", num.trees = 500, mtry = 8, min.bucket = 3, max.depth = 20)

rf_sp_oc_no2 <- rf_sp_avg("NO2", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 10)
rf_sp_oc_pm10 <- rf_sp_avg("PM10", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 15)
rf_sp_oc_pm25 <- rf_sp_avg("PM25", num.trees = 500, mtry = 8, min.bucket = 3, max.depth = 20)

rf_oc_cv <- as.data.frame(rbind(rf_oc_no2$CV, rf_oc_pm10$CV, rf_oc_pm25$CV))
rownames(rf_oc_cv) <- c("rf_oc_no2", "rf_oc_pm10", "rf_oc_pm25")

rf_sp_oc_cv <- as.data.frame(rbind(rf_sp_oc_no2$CV, rf_sp_oc_pm10$CV, rf_sp_oc_pm25$CV))
rownames(rf_sp_oc_cv) <- c("rf_sp_oc_no2", "rf_sp_oc_pm10", "rf_sp_oc_pm25")

rflog_avg <- function(p = "NO2", num.trees, mtry, min.bucket, max.depth) {
  cl <- makeCluster(nCl)
  registerDoParallel(cl)

  hyper_grid_rf <- expand.grid(
    Split = paste0("Split", 1:10),
    num.trees = num.trees,
    mtry = mtry,
    min.bucket = min.bucket,
    max.depth = max.depth
  )

  data <- get(paste0("data_", str_to_lower(p))) %>%
    rename(logy = paste0("log", p)) %>%
    filter(!is.na(logy))

  # execute full cartesian grid
  pred_df <- foreach(
    i = 1:nrow(hyper_grid_rf),
    .packages = c("tidyverse", "ranger"),
    .export = c("rmse", "mae", "bias", "rflog"),
    .combine = rbind
  ) %dopar% {
    split_str <- as.character(hyper_grid_rf$Split[i])
    data_train <- data %>% filter(get(split_str) != "test")
    data_test <- data %>% filter(get(split_str) == "test")

    rf_fit <- rflog(
      formula = as.formula(
        paste(
          "logy",
          paste("Type", "UR", "Year", "Month", paste0("log", p, "_modelled"),
                "Hurs", "Psl", "SfcWind", "Sun", "Rainfall", "Tas", "NDVI",
                "log_nearest_road_distance",
                sep = " + "),
          sep = " ~ "
        )
      ),
      train_data = data_train,
      test_data = data_test,
      alpha = 0.05,
      params_ranger = list(
        num.trees = hyper_grid_rf$num.trees[i],
        mtry = hyper_grid_rf$mtry[i],
        min.bucket = hyper_grid_rf$min.bucket[i],
        max.depth = hyper_grid_rf$max.depth[i],
        verbose = FALSE,
        seed = 123,
        respect.unordered.factors = "order"
      )
    )

    data_test %>%
      rename(Obs = paste0(p)) %>%
      dplyr::select(Year_Month, Site, Obs) %>%
      mutate(Pred = exp(rf_fit$testPred + rf_fit$sigma^2/2),
             Lower = exp(rf_fit$interval$lower),
             Upper = exp(rf_fit$interval$upper),
             Split = i)
  }

  cv_df <- pred_df %>%
    dplyr::select(Obs, Pred, Lower, Upper, Split) %>%
    group_by(Split) %>%
    summarise(RMSE = rmse(Obs, Pred),
              MAE = mae(Obs, Pred),
              Bias = bias(Obs, Pred),
              Coverage = coverage(Obs, Lower, Upper),
              AIW = aiw(Lower, Upper))

  return(list(
    CV = apply(cv_df %>% select(-Split), 2, mean),
    CV_Split = cv_df,
    Pred = pred_df
  ))

  stopCluster(cl)
}

rflog_sp_avg <- function(p = "NO2", num.trees, mtry, min.bucket, max.depth) {
  cl <- makeCluster(nCl)
  registerDoParallel(cl)

  hyper_grid_rf <- expand.grid(
    Split = paste0("Split", 1:10),
    num.trees = num.trees,
    mtry = mtry,
    min.bucket = min.bucket,
    max.depth = max.depth
  )

  data <- get(paste0("data_", str_to_lower(p))) %>%
    rename(logy = paste0("log", p)) %>%
    filter(!is.na(logy))

  # execute full cartesian grid
  pred_df <- foreach(
    i = 1:nrow(hyper_grid_rf),
    .packages = c("tidyverse", "ranger"),
    .export = c("rmse", "mae", "bias", "rflog"),
    .combine = rbind
  ) %dopar% {
    split_str <- as.character(hyper_grid_rf$Split[i])
    data_train <- data %>% filter(get(split_str) != "test")
    data_test <- data %>% filter(get(split_str) == "test")

    rf_fit <- rflog(
      formula = as.formula(
        paste(
          "logy",
          paste("Type", "UR", "Year", "Month", paste0("log", p, "_modelled"), "Longitude",  "Latitude",
                "Hurs", "Psl", "SfcWind", "Sun", "Rainfall", "Tas", "NDVI",
                "log_nearest_road_distance",
                sep = " + "),
          sep = " ~ "
        )
      ),
      train_data = data_train,
      test_data = data_test,
      alpha = 0.05,
      params_ranger = list(
        num.trees = hyper_grid_rf$num.trees[i],
        mtry = hyper_grid_rf$mtry[i],
        min.bucket = hyper_grid_rf$min.bucket[i],
        max.depth = hyper_grid_rf$max.depth[i],
        verbose = FALSE,
        seed = 123,
        respect.unordered.factors = "order"
      )
    )

    data_test %>%
      rename(Obs = paste0(p)) %>%
      dplyr::select(Year_Month, Site, Obs) %>%
      mutate(Pred = exp(rf_fit$testPred + rf_fit$sigma^2/2),
             Lower = exp(rf_fit$interval$lower),
             Upper = exp(rf_fit$interval$upper),
             Split = i)
  }

  cv_df <- pred_df %>%
    dplyr::select(Obs, Pred, Lower, Upper, Split) %>%
    group_by(Split) %>%
    summarise(RMSE = rmse(Obs, Pred),
              MAE = mae(Obs, Pred),
              Bias = bias(Obs, Pred),
              Coverage = coverage(Obs, Lower, Upper),
              AIW = aiw(Lower, Upper))

  return(list(
    CV = apply(cv_df %>% select(-Split), 2, mean),
    CV_Split = cv_df,
    Pred = pred_df
  ))

  stopCluster(cl)
}

rf_lc_no2 <- rflog_avg("NO2", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 10)
rf_lc_pm10 <- rflog_avg("PM10", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 15)
rf_lc_pm25 <- rflog_avg("PM25", num.trees = 500, mtry = 8, min.bucket = 3, max.depth = 20)

rf_sp_lc_no2 <- rflog_sp_avg("NO2", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 10)
rf_sp_lc_pm10 <- rflog_sp_avg("PM10", num.trees = 500, mtry = 4, min.bucket = 3, max.depth = 15)
rf_sp_lc_pm25 <- rflog_sp_avg("PM25", num.trees = 500, mtry = 8, min.bucket = 3, max.depth = 20)

rf_lc_cv <- as.data.frame(rbind(rf_lc_no2$CV, rf_lc_pm10$CV, rf_lc_pm25$CV))
rownames(rf_lc_cv) <- c("rf_lc_no2", "rf_lc_pm10", "rf_lc_pm25")

rf_sp_lc_cv <- as.data.frame(rbind(rf_sp_lc_no2$CV, rf_sp_lc_pm10$CV, rf_sp_lc_pm25$CV))
rownames(rf_sp_lc_cv) <- c("rf_sp_lc_no2", "rf_sp_lc_pm10", "rf_sp_lc_pm25")

cv_rf <- rbind(rf_oc_cv, rf_sp_oc_cv, rf_lc_cv, rf_sp_lc_cv)

save(cv_rf,
     rf_oc_no2, rf_oc_pm10, rf_oc_pm25,
     rf_sp_oc_no2, rf_sp_oc_pm10, rf_sp_oc_pm25,
     rf_lc_no2, rf_lc_pm10, rf_lc_pm25,
     rf_sp_lc_no2, rf_sp_lc_pm10, rf_sp_lc_pm25,
     file = "./Data/Results - Random Forests.RData")
