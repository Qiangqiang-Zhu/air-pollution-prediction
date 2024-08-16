#### R script to build statistical models including linear models, additive models and hierarchical models

library(tidyverse)
library(data.table)
library(mgcv)
library(progress)
library(spTimer)
library(spTDyn)

data_no2 <- fread("./Data/Data - NO2.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas"))
  ) %>%
  arrange(Site, Year_Month)

data_pm10 <- fread("./Data/Data - PM10.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas"))
  ) %>%
  arrange(Site, Year_Month)

data_pm25 <- fread("./Data/Data - PM25.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas"))
  ) %>%
  arrange(Site, Year_Month)

## Root mean squared error
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))

## Median absolute error
mae <- function(y, yhat) median(abs(y - yhat), na.rm = TRUE)

## Bias
bias <- function(y, yhat) mean(yhat - y, na.rm = TRUE)

## Coverage probability
coverage <- function(y, lwr, upr) mean(y >= lwr & y <= upr)

## Average interval width
aiw <- function(lwr, upr) mean(abs(upr - lwr))

## Normal linear models --------------------------------------------------------

lm_avg <- function(f, data) {
  cv_mat <- matrix(0, nrow = 10, ncol = 5)

  pred_df <- data.frame(Year_Month = NULL, Site = NULL, Obs = NULL, Pred = NULL,
                        Lwr = NULL, Upr = NULL, Split = NULL)

  for (i in 1:10) {
    split_str <- paste0("Split", i)
    data_train <- data %>% filter(get(split_str) != "test")
    site_list <- unique(data$Site)
    if ("Inverness Academy Street 1st Floor" %in% site_list &&
        "Inverness Academy Street"  %in% site_list) {
      data_train <- data_train %>% filter(Site != "Inverness Academy Street 1st Floor")
    }

    data_test <- data %>% filter(get(split_str) == "test")
    lm_fit <- lm(formula = f, data = data_train)
    lm_sigma <- summary(lm_fit)$sigma
    pred <- predict(lm_fit, data_test, interval = "prediction")
    pred_y <- exp(pred[, 1] + lm_sigma^2 / 2)
    y_name <- paste0(gsub("log", "", f[[2]]))

    lm_pred_df <- data.frame(
      Year_Month = data_test$Year_Month,
      Site = data_test$Site,
      Obs = data_test[[y_name]],
      Pred = pred_y,
      Lwr = exp(pred[, 2]),
      Upr = exp(pred[, 3]),
      Split = i
    )
    pred_df <- rbind(pred_df, lm_pred_df)

    cv_mat[i, 1] <- rmse(lm_pred_df$Obs, lm_pred_df$Pred)
    cv_mat[i, 2] <- mae(lm_pred_df$Obs, lm_pred_df$Pred)
    cv_mat[i, 3] <- bias(lm_pred_df$Obs, lm_pred_df$Pred)
    cv_mat[i, 4] <- coverage(lm_pred_df$Obs, lm_pred_df$Lwr, lm_pred_df$Upr)
    cv_mat[i, 5] <- aiw(lm_pred_df$Lwr, lm_pred_df$Upr)
  }

  cv_df <- as.data.frame(cv_mat)
  names(cv_df) <- c("RMSE", "MAE", "Bias", "Coverage", "AIW")

  return(list(CV = apply(cv_df, 2, mean), Pred = pred_df))
}

lm_cs_no2 <- lm_avg(
  logNO2 ~ Type + UR + Year + as.factor(Month) + log(NO2_modelled) +
    Hurs + Psl + SfcWind + Sun + Tas + Rainfall + NDVI +
    PopDen + log(nearest_road_distance),
  data = data_no2 %>% filter(!is.na(logNO2))
)
lm_vs_no2 <- lm_avg(
  logNO2 ~ Type + UR + Year_Month + log(NO2_modelled) +
    Hurs + Psl + SfcWind + Sun + Tas + Rainfall + NDVI +
    PopDen + log(nearest_road_distance),
  data = data_no2 %>% filter(!is.na(logNO2))
)

lm_cs_pm10 <- lm_avg(
  logPM10 ~ Type + UR + Year + as.factor(Month) +log(PM10_modelled) +
    Hurs + Psl + SfcWind + Sun + Tas + Rainfall + NDVI +
    PopDen + log(nearest_road_distance),
  data = data_pm10 %>% filter(!is.na(logPM10))
)
lm_vs_pm10 <- lm_avg(
  logPM10 ~ Type + UR + Year_Month + log(PM10_modelled) +
    Hurs + Psl + SfcWind + Sun + Tas + Rainfall + NDVI +
    PopDen + log(nearest_road_distance),
  data = data_pm10 %>% filter(!is.na(logPM10))
)

lm_cs_pm25 <- lm_avg(
  logPM25 ~ Type + UR + Year + as.factor(Month) + log(PM25_modelled) +
    Hurs + Psl + SfcWind + Sun + Tas + Rainfall + NDVI +
    PopDen + log(nearest_road_distance),
  data = data_pm25 %>% filter(!is.na(logPM25))
)
lm_vs_pm25 <- lm_avg(
  logPM25 ~ Type + UR + Year_Month + log(PM25_modelled) +
    Hurs + Psl + SfcWind + Sun + Tas + Rainfall + NDVI +
    PopDen + log(nearest_road_distance),
  data = data_pm25 %>% filter(!is.na(logPM25))
)

lm_cv <- as.data.frame(rbind(lm_cs_no2$CV, lm_vs_no2$CV, lm_cs_pm10$CV, lm_vs_pm10$CV, lm_cs_pm25$CV, lm_vs_pm25$CV))
rownames(lm_cv) <- c("lm_cs_no2", "lm_vs_no2", "lm_cs_pm10", "lm_vs_pm10", "lm_cs_pm25", "lm_vs_pm25")
lm_cv

## Additive models -------------------------------------------------------------

am_avg <- function(f, data, knots = NULL, func = "gam") {
  cv_mat <- matrix(0, nrow = 10, ncol = 5)

  pred_df <- data.frame(Year_Month = NULL, Site = NULL, Obs = NULL, Pred = NULL,
                        Lwr = NULL, Upr = NULL, Split = NULL)

  pb <- progress_bar$new(
    format = "Progressing [:bar] :percent in :elapsed, eta: :eta",
    total = 10, clear = FALSE, width = 80
  )
  for (i in 1:10) {
    pb$tick()
    split_str <- paste0("Split", i)
    data_train <- data %>% filter(get(split_str) != "test")
    site_list <- unique(data$Site)
    if ("Inverness Academy Street 1st Floor" %in% site_list &&
        "Inverness Academy Street"  %in% site_list) {
      data_train <- data_train %>% filter(Site != "Inverness Academy Street 1st Floor")
    }
    data_test <- data %>% filter(get(split_str) == "test")
    if (func != "bam") {
      am_fit <- gam(formula = f, data = data_train, method = "REML", knots = knots)
    } else {
      am_fit <- bam(formula = f, data = data_train, method = "fREML", discrete = TRUE, knots = knots)
    }
    am_sig2 <- am_fit$sig2

    pred <- predict(am_fit, data_test, se.fit = TRUE)
    pred_mat <- matrix(0, nrow = nrow(data_test), ncol = 3)
    pred_mat[, 1] <- pred$fit
    pred_se <- sqrt(am_sig2 + pred$se.fit^2)
    pred_mat[, 2] <- pred$fit - 1.96 * pred_se
    pred_mat[, 3] <- pred$fit + 1.96 * pred_se
    pred_y <- exp(pred_mat[, 1] + am_sig2 / 2)
    y_name <- paste0(gsub("log", "", f[[2]]))

    am_pred_df <- data.frame(
      Year_Month = data_test$Year_Month,
      Site = data_test$Site,
      Obs = data_test[[y_name]],
      Pred = pred_y,
      Lwr = exp(pred_mat[, 2]),
      Upr = exp(pred_mat[, 3]),
      Split = i
    )
    pred_df <- rbind(pred_df, am_pred_df)

    cv_mat[i, 1] <- rmse(am_pred_df$Obs, am_pred_df$Pred)
    cv_mat[i, 2] <- mae(am_pred_df$Obs, am_pred_df$Pred)
    cv_mat[i, 3] <- bias(am_pred_df$Obs, am_pred_df$Pred)
    cv_mat[i, 4] <- coverage(am_pred_df$Obs, am_pred_df$Lwr, am_pred_df$Upr)
    cv_mat[i, 5] <- aiw(am_pred_df$Lwr, am_pred_df$Upr)
  }

  cv_df <- as.data.frame(cv_mat)
  names(cv_df) <- c("RMSE", "MAE", "Bias", "Coverage", "AIW")

  return(list(CV = apply(cv_df, 2, mean), Pred = pred_df))
}

### AMs with GP splines for the temporal trend ---------------------------------

am_no2 <- am_avg(
  logNO2 ~ Type + UR +
    s(log(NO2_modelled), bs = "ps", m = 1) +
    s(Hurs, bs = "ps", m = 1) +
    s(Psl, bs = "ps", m = 1) +
    s(SfcWind, bs = "ps", m = 1) +
    s(Sun, bs = "ps", m = 1) +
    s(Tas, bs = "ps", m = 1) +
    s(Rainfall, bs = "ps", m = 1) +
    s(NDVI, bs = "ps", m = 1) +
    s(PopDen, bs = "ps", m = 1) +
    s(log(nearest_road_distance), bs = "ps", m = 1) +
    s(Temp_Trend, bs = "gp", m = 1, k = 60) +
    s(Month, bs = "cc", k = 13),
  data = data_no2 %>% filter(!is.na(logNO2)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60),
)

am_pm10 <- am_avg(
  logPM10 ~ Type + UR +
    s(log(PM10_modelled), bs = "ps", m = 1) +
    s(Hurs, bs = "ps", m = 1) +
    s(Psl, bs = "ps", m = 1) +
    s(SfcWind, bs = "ps", m = 1) +
    s(Sun, bs = "ps", m = 1) +
    s(Tas, bs = "ps", m = 1) +
    s(Rainfall, bs = "ps", m = 1) +
    s(NDVI, bs = "ps", m = 1) +
    s(PopDen, bs = "ps", m = 1) +
    s(log(nearest_road_distance), bs = "ps", m = 1) +
    s(Temp_Trend, bs = "gp", m = 1, k = 60) +
    s(Month, bs = "cc", k = 13),
  data = data_pm10 %>% filter(!is.na(logPM10)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Tend = 1:60),
)

am_pm25 <- am_avg(
  logPM25 ~ Type + UR +
    s(log(PM25_modelled), bs = "ps", m = 1) +
    s(Hurs, bs = "ps", m = 1) +
    s(Psl, bs = "ps", m = 1) +
    s(SfcWind, bs = "ps", m = 1) +
    s(Sun, bs = "ps", m = 1) +
    s(Tas, bs = "ps", m = 1) +
    s(Rainfall, bs = "ps", m = 1) +
    s(NDVI, bs = "ps", m = 1) +
    s(PopDen, bs = "ps", m = 1) +
    s(log(nearest_road_distance), bs = "ps", m = 1) +
    s(Temp_Trend, bs = "gp", m = 1, k = 60) +
    s(Month, bs = "cc", k = 13),
  data = data_pm25 %>% filter(!is.na(logPM25)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

am_cv <- as.data.frame(rbind(am_no2$CV, am_pm10$CV, am_pm25$CV))
rownames(am_cv) <- c("am_no2", "am_pm10", "am_pm25")
am_cv


### AMs with the spatial terms -------------------------------------------------

am_sp_no2 <- am_avg(
  logNO2 ~ Type + UR +
    s(log(NO2_modelled), bs = "ps", m = 1) +
    s(Longitude, Latitude) +
    s(Hurs, bs = "ps", m = 1) +
    s(Psl, bs = "ps", m = 1) +
    s(SfcWind, bs = "ps", m = 1) +
    s(Sun, bs = "ps", m = 1) +
    s(Tas, bs = "ps", m = 1) +
    s(Rainfall, bs = "ps", m = 1) +
    s(NDVI, bs = "ps", m = 1) +
    s(PopDen, bs = "ps", m = 1) +
    s(log(nearest_road_distance), bs = "ps", m = 1) +
    s(Temp_Trend, bs = "gp", m = 1, k = 60) +
    s(Month, bs = "cc", k = 13),
  data = data_no2 %>% filter(!is.na(logNO2)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

am_sp_pm10 <- am_avg(
  logPM10 ~ Type + UR +
    s(log(PM10_modelled), bs = "ps", m = 1) +
    s(Longitude, Latitude) +
    s(Hurs, bs = "ps", m = 1) +
    s(Psl, bs = "ps", m = 1) +
    s(SfcWind, bs = "ps", m = 1) +
    s(Sun, bs = "ps", m = 1) +
    s(Tas, bs = "ps", m = 1) +
    s(Rainfall, bs = "ps", m = 1) +
    s(NDVI, bs = "ps", m = 1) +
    s(PopDen, bs = "ps", m = 1) +
    s(log(nearest_road_distance), bs = "ps", m = 1) +
    s(Temp_Trend, bs = "gp", m = 1, k = 60) +
    s(Month, bs = "cc", k = 13),
  data = data_pm10 %>% filter(!is.na(logPM10)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

am_sp_pm25 <- am_avg(
  logPM25 ~ Type + UR +
    s(log(PM25_modelled), bs = "ps", m = 1) +
    s(Longitude, Latitude) +
    s(Hurs, bs = "ps", m = 1) +
    s(Psl, bs = "ps", m = 1) +
    s(SfcWind, bs = "ps", m = 1) +
    s(Sun, bs = "ps", m = 1) +
    s(Tas, bs = "ps", m = 1) +
    s(Rainfall, bs = "ps", m = 1) +
    s(NDVI, bs = "ps", m = 1) +
    s(PopDen, bs = "ps", m = 1) +
    s(log(nearest_road_distance), bs = "ps", m = 1) +
    s(Temp_Trend, bs = "gp", m = 1, k = 60) +
    s(Month, bs = "cc", k = 13),
  data = data_pm25 %>% filter(!is.na(logPM25)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

am_sp_cv <- as.data.frame(rbind(am_sp_no2$CV, am_sp_pm10$CV, am_sp_pm25$CV))
rownames(am_sp_cv) <- c("am_sp_no2", "am_sp_pm10", "am_sp_pm25")
am_sp_cv

## Additive modes for location, scale and shape --------------------------------

amlss_avg <- function(f, data, knots = NULL, method = "REML") {
  cv_mat <- matrix(0, nrow = 10, ncol = 5)

  pred_df <- data.frame(Year_Month = NULL, Site = NULL, Obs = NULL, Pred = NULL,
                        Lwr = NULL, Upr = NULL, Split = NULL)

  pb <- progress_bar$new(
    format = "Progressing [:bar] :percent in :elapsed, eta: :eta",
    total = 10, clear = FALSE, width = 80
  )
  for (i in 1:10) {
    pb$tick()
    split_str <- paste0("Split", i)
    data_train <- data %>% filter(get(split_str) != "test")
    site_list <- unique(data$Site)
    if ("Inverness Academy Street 1st Floor" %in% site_list &&
        "Inverness Academy Street"  %in% site_list) {
      data_train <- data_train %>% filter(Site != "Inverness Academy Street 1st Floor")
    }
    data_test <- data %>% filter(get(split_str) == "test")
    amlss_fit <- gam(formula = f, data = data_train, family = "gaulss", knots = knots, method = method)
    pred <- predict(amlss_fit, data_test, se.fit = TRUE, type = "response")
    pred_mat <- matrix(0, nrow = nrow(data_test), ncol = 3)
    pred_mat[, 1] <- pred$fit[, 1]
    pred_se <- sqrt((1 / pred$fit[, 2])^2 + pred$se.fit[, 1]^2)
    pred_mat[, 2] <- pred$fit[, 1] - 1.96 * pred_se
    pred_mat[, 3] <- pred$fit[, 1] + 1.96 * pred_se
    pred_y <- exp(pred_mat[, 1] + pred$se.fit[, 1]^2 / 2)
    y_name <- paste0(gsub("log", "", f[[1]][[2]]))

    amlss_pred_df <- data.frame(
      Year_Month = data_test$Year_Month,
      Site = data_test$Site,
      Obs = data_test[[y_name]],
      Pred = pred_y,
      Lwr = exp(pred_mat[, 2]),
      Upr = exp(pred_mat[, 3]),
      Split = i
    )
    pred_df <- rbind(pred_df, amlss_pred_df)

    cv_mat[i, 1] <- rmse(amlss_pred_df$Obs, amlss_pred_df$Pred)
    cv_mat[i, 2] <- mae(amlss_pred_df$Obs, amlss_pred_df$Pred)
    cv_mat[i, 3] <- bias(amlss_pred_df$Obs, amlss_pred_df$Pred)
    cv_mat[i, 4] <- coverage(amlss_pred_df$Obs, amlss_pred_df$Lwr, amlss_pred_df$Upr)
    cv_mat[i, 5] <- aiw(amlss_pred_df$Lwr, amlss_pred_df$Upr)
  }

  cv_df <- as.data.frame(cv_mat)
  names(cv_df) <- c("RMSE", "MAE", "Bias", "Coverage", "AIW")

  return(list(CV = apply(cv_df, 2, mean), Pred = pred_df))
}

### AMLSS with GP splines for the temporal trend -------------------------------

amlss_no2 <- amlss_avg(
  list(logNO2 ~ Type + UR +
         s(log(NO2_modelled), bs = "ps", m = 1) +
         s(Hurs, bs = "ps", m = 1) +
         s(Psl, bs = "ps", m = 1) +
         s(SfcWind, bs = "ps", m = 1) +
         s(Sun, bs = "ps", m = 1) +
         s(Tas, bs = "ps", m = 1) +
         s(Rainfall, bs = "ps", m = 1) +
         s(NDVI, bs = "ps", m = 1) +
         s(PopDen, bs = "ps", m = 1) +
         s(log(nearest_road_distance), bs = "ps", m = 1) +
         s(Temp_Trend, bs = "gp", m = 1, k = 60) +
         s(Month, bs = "cc", k = 13),
       ~ s(NO2_NA_no, bs = "ps", m = 1)),
  data = data_no2 %>% filter(!is.na(logNO2)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

amlss_pm10 <- amlss_avg(
  list(logPM10 ~ Type + UR +
         s(log(PM10_modelled), bs = "ps", m = 1) +
         s(Hurs, bs = "ps", m = 1) +
         s(Psl, bs = "ps", m = 1) +
         s(SfcWind, bs = "ps", m = 1) +
         s(Sun, bs = "ps", m = 1) +
         s(Tas, bs = "ps", m = 1) +
         s(Rainfall, bs = "ps", m = 1) +
         s(NDVI, bs = "ps", m = 1) +
         s(PopDen, bs = "ps", m = 1) +
         s(log(nearest_road_distance), bs = "ps", m = 1) +
         s(Temp_Trend, bs = "gp", m = 1, k = 60) +
         s(Month, bs = "cc", k = 13),
       ~ s(PM10_NA_no, bs = "ps", m = 1)),
  data = data_pm10 %>% filter(!is.na(logPM10)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

amlss_pm25 <- amlss_avg(
  list(logPM25 ~ Type + UR +
         s(log(PM25_modelled), bs = "ps", m = 1) +
         s(Hurs, bs = "ps", m = 1) +
         s(Psl, bs = "ps", m = 1) +
         s(SfcWind, bs = "ps", m = 1) +
         s(Sun, bs = "ps", m = 1) +
         s(Tas, bs = "ps", m = 1) +
         s(Rainfall, bs = "ps", m = 1) +
         s(NDVI, bs = "ps", m = 1) +
         s(PopDen, bs = "ps", m = 1) +
         s(log(nearest_road_distance), bs = "ps", m = 1) +
         s(Temp_Trend, bs = "gp", m = 1, k = 60) +
         s(Month, bs = "cc", k = 13),
       ~ s(PM25_NA_no, bs = "ps", m = 1)),
  data = data_pm25 %>% filter(!is.na(logPM25)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

amlss_cv <- as.data.frame(rbind(amlss_no2$CV, amlss_pm10$CV, amlss_pm25$CV))
row.names(amlss_cv) <- c("amlss_no2", "amlss_pm10", "amlss_pm25")
amlss_cv

### AMLSS with the spatial terms -----------------------------------------------

amlss_sp_no2 <- amlss_avg(
  list(logNO2 ~ Type + UR +
         s(log(NO2_modelled), bs = "ps", m = 1) +
         s(Longitude, Latitude) +
         s(Hurs, bs = "ps", m = 1) +
         s(Psl, bs = "ps", m = 1) +
         s(SfcWind, bs = "ps", m = 1) +
         s(Sun, bs = "ps", m = 1) +
         s(Tas, bs = "ps", m = 1) +
         s(Rainfall, bs = "ps", m = 1) +
         s(NDVI, bs = "ps", m = 1) +
         s(PopDen, bs = "ps", m = 1) +
         s(log(nearest_road_distance), bs = "ps", m = 1) +
         s(Temp_Trend, bs = "gp", m = 1, k = 60) +
         s(Month, bs = "cc", k = 13),
       ~ s(NO2_NA_no, bs = "ps", m = 1)),
  data = data_no2 %>% filter(!is.na(logNO2)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

amlss_sp_pm10 <- amlss_avg(
  list(logPM10 ~ Type + UR +
         s(log(PM10_modelled), bs = "ps", m = 1) +
         s(Longitude, Latitude) +
         s(Hurs, bs = "ps", m = 1) +
         s(Psl, bs = "ps", m = 1) +
         s(SfcWind, bs = "ps", m = 1) +
         s(Sun, bs = "ps", m = 1) +
         s(Tas, bs = "ps", m = 1) +
         s(Rainfall, bs = "ps", m = 1) +
         s(NDVI, bs = "ps", m = 1) +
         s(PopDen, bs = "ps", m = 1) +
         s(log(nearest_road_distance), bs = "ps", m = 1) +
         s(Temp_Trend, bs = "gp", m = 1, k = 60) +
         s(Month, bs = "cc", k = 13),
       ~ s(PM10_NA_no, bs = "ps", m = 1)),
  data = data_pm10 %>% filter(!is.na(logPM10)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

amlss_sp_pm25 <- amlss_avg(
  list(logPM25 ~ Type + UR +
         s(log(PM25_modelled), bs = "ps", m = 1) +
         s(Longitude, Latitude) +
         s(Hurs, bs = "ps", m = 1) +
         s(Psl, bs = "ps", m = 1) +
         s(SfcWind, bs = "ps", m = 1) +
         s(Sun, bs = "ps", m = 1) +
         s(Tas, bs = "ps", m = 1) +
         s(Rainfall, bs = "ps", m = 1) +
         s(NDVI, bs = "ps", m = 1) +
         s(PopDen, bs = "ps", m = 1) +
         s(log(nearest_road_distance), bs = "ps", m = 1) +
         s(Temp_Trend, bs = "gp", m = 1, k = 60) +
         s(Month, bs = "cc", k = 13),
       ~ s(PM25_NA_no, bs = "ps", m = 1)),
  data = data_pm25 %>% filter(!is.na(logPM25)),
  knots = list(Month = seq(0, 12, length = 13), Temp_Trend = 1:60)
)

amlss_sp_cv <- as.data.frame(rbind(amlss_sp_no2$CV, amlss_sp_pm10$CV, amlss_sp_pm25$CV))
row.names(amlss_sp_cv) <- c("amlss_sp_no2", "amlss_sp_pm10", "amlss_sp_pm25")
amlss_sp_cv

## Hierarchical spatio-temporal models -----------------------------------------

spm_avg <- function(f, data, model = "GP") {
  cv_mat <- matrix(0, nrow = 10, ncol = 5)

  pred_df <- data.frame(Year_Month = NULL, Site = NULL, Obs = NULL, Pred = NULL,
                        Lwr = NULL, Upr = NULL, Split = NULL)

  for (i in 1:10) {
    cat(i, " ")
    split_str <- paste0("Split", i)
    data_train <- data %>% filter(get(split_str) != "test")
    site_list <- unique(data$Site)
    if ("Inverness Academy Street 1st Floor" %in% site_list &&
        "Inverness Academy Street"  %in% site_list) {
      data_train <- data_train %>% filter(Site != "Inverness Academy Street 1st Floor")
    }
    data_test <- data %>% filter(get(split_str) == "test")

    spm_fit <- spT.Gibbs(
      formula = f,
      data = data_train,
      model = model,
      coords = ~ Easting + Northing,
      nItr = 10000,
      nBurn = 2000,
      # tol.dist = 0.05,
      scale.transform = "LOG",
      spatial.decay = decay(distribution = Gamm(2, 1), tuning = 0.06)
    )

    pred <- predict(spm_fit, newcoords = ~ Easting + Northing, newdata = data_test)

    spm_pred_df <- data.frame(
      Year_Month = data_test$Year_Month,
      Site = data_test$Site,
      Obs = data_test[[f[[2]]]],
      Pred = as.vector(pred$Median),
      Lwr = as.vector(pred$Low),
      Upr = as.vector(pred$Up),
      Split = i
    )
    spm_pred_df <- spm_pred_df %>% dplyr::filter(!is.na(Obs))
    pred_df <- rbind(pred_df, spm_pred_df)

    cv_mat[i, 1] <- rmse(spm_pred_df$Obs, spm_pred_df$Pred)
    cv_mat[i, 2] <- mae(spm_pred_df$Obs, spm_pred_df$Pred)
    cv_mat[i, 3] <- bias(spm_pred_df$Obs, spm_pred_df$Pred)
    cv_mat[i, 4] <- coverage(spm_pred_df$Obs, spm_pred_df$Lwr, spm_pred_df$Upr)
    cv_mat[i, 5] <- aiw(spm_pred_df$Lwr, spm_pred_df$Upr)
  }

  cv_df <- as.data.frame(cv_mat)
  names(cv_df) <- c("RMSE", "MAE", "Bias", "Coverage", "AIW")

  return(list(CV = apply(cv_df, 2, mean), Pred = pred_df))
}

### Independent GP models with nugget effect -----------------------------------

sp_gp_no2 <- spm_avg(
  f = NO2 ~ Type + UR + log(NO2_modelled) + Hurs + Psl + SfcWind + Sun + Tas +
    Rainfall + NDVI + PopDen + log(nearest_road_distance),
  data = data_no2
)

sp_gp_pm10 <- spm_avg(
  f = PM10 ~ Type + UR + log(PM10_modelled) + Hurs + Psl + SfcWind + Sun + Tas +
    Rainfall + NDVI + PopDen + log(nearest_road_distance),
  data = data_pm10
)

sp_gp_pm25 <- spm_avg(
  f = PM25 ~ Type + UR + log(PM25_modelled) + Hurs + Psl + SfcWind + Sun + Tas +
    Rainfall + NDVI + PopDen + log(nearest_road_distance),
  data = data_pm25
)

sp_gp_cv <- as.data.frame(rbind(sp_gp_no2$CV, sp_gp_pm10$CV, sp_gp_pm25$CV))
rownames(sp_gp_cv) <- c("sp_gp_no2", "sp_gp_pm10", "sp_gp_pm25")
sp_gp_cv

### Autoregressive models ------------------------------------------------------

sp_ar_no2 <- spm_avg(
  f = NO2 ~ Type + UR + log(NO2_modelled) + Hurs + Psl + SfcWind + Sun + Tas +
    Rainfall + NDVI + PopDen + log(nearest_road_distance),
  data = data_no2, model = "AR"
)

sp_ar_pm10 <- spm_avg(
  f = PM10 ~ Type + UR + log(PM10_modelled) + Hurs + Psl + SfcWind + Sun + Tas +
    Rainfall + NDVI + PopDen + log(nearest_road_distance),
  data = data_pm10, model = "AR"
)

sp_ar_pm25 <- spm_avg(
  f = PM25 ~ Type + UR + log(PM25_modelled) + Hurs + Psl + SfcWind + Sun + Tas +
    Rainfall + NDVI + PopDen + log(nearest_road_distance),
  data = data_pm25, model = "AR"
)

sp_ar_cv <- as.data.frame(rbind(sp_ar_no2$CV, sp_ar_pm10$CV, sp_ar_pm25$CV))
rownames(sp_ar_cv) <- c("sp_ar_no2", "sp_ar_pm10", "sp_ar_pm25")
ar_cv

cv_stat_models <- rbind(lm_cv, am_cv, am_sp_cv, amlss_cv, amlss_sp_cv, sp_gp_cv, sp_ar_cv)
cv_stat_models

save.image("./Data/Results - Statistical Models.RData")
