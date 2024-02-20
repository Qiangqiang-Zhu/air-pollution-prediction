#### R script to predict monthly air pollutant concentrations across Scotland
#### using RF_log for NO2, LM_vs for PM10 and RF_log for PM2.5

library(tidyverse)
library(ranger)
library(data.table)
library(rfinterval)

source("rflog.R")

# Data preparation -------------------------------------------------------------

data_no2 <- read.csv("Data - NO2.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas"))
  ) %>%
  filter(!is.na(NO2))

data_pm10 <- read.csv("Data - PM10.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas"))
  ) %>%
  filter(!is.na(PM10))

data_pm25 <- read.csv("Data - PM25.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas"))
  ) %>%
  filter(!is.na(PM25))

## "Preidction Data.csv" contains all predictors for prediction in Scotland.
pred_data <- fread("Prediction Data.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas"))
  )

# Train models -----------------------------------------------------------------

## NO2

data_no2 <- data_no2 %>% mutate(logNO2_modelled = log(NO2_modelled))
pred_data <- pred_data %>% mutate(logNO2_modelled = log(NO2_modelled))

rf_fit_no2 <- rflog(
  formula = logNO2 ~ Type + UR + Year + Month + logNO2_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI,
  train_data = data_no2,
  test_data = pred_data,
  alpha = 0.05,
  params_ranger = list(
    num.trees = 500,
    mtry = 4,
    min.bucket = 3,
    max.depth = 10,
    verbose = FALSE,
    seed = 123,
    respect.unordered.factors = "order"
  )
)

pred_results <- pred_data %>%
  select(-c(Hurs, Psl, Rainfall, SfcWind, Sun, Tas, NDVI)) %>%
  mutate(
    pred_no2_rf = exp(rf_fit_no2$testPred + rf_fit_no2$sigma^2/2),
    lwr_no2_rf = exp(rf_fit_no2$interval$lower),
    upr_no2_rf = exp(rf_fit_no2$interval$upper)
  )


## PM10

lmvs_fit_pm10 <- lm(
  logPM10 ~ Type + UR + Year_Month + log(PM10_modelled) +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI,
  data = data_pm10
)
lmvs_sigma_pm10 <- summary(lmvs_fit_pm10)$sigma
pred_lmvs_pm10 <- predict(lmvs_fit_pm10, pred_data, interval = "prediction")
pred_results <- pred_results %>%
  mutate(
    pred_pm10_lmvs = exp(pred_lmvs_pm10[, 1] + lmvs_sigma_pm10^2/2),
    lwr_pm10_lmvs = exp(pred_lmvs_pm10[, 2]),
    upr_pm10_lmvs = exp(pred_lmvs_pm10[, 3])
  )


## PM25

data_pm25 <- data_pm25 %>% mutate(logPM25_modelled = log(PM25_modelled))
pred_data <- pred_data %>% mutate(logPM25_modelled = log(PM25_modelled))

rf_fit_pm25 <- rflog(
  formula = logPM25 ~ Type + UR + Year + Month + logPM25_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI,
  train_data = data_pm25,
  test_data = pred_data,
  alpha = 0.05,
  params_ranger = list(
    num.trees = 500,
    mtry = 8,
    min.bucket = 3,
    max.depth = 20,
    verbose = FALSE,
    seed = 123,
    respect.unordered.factors = "order"
  )
)

pred_results <- pred_results %>%
  mutate(
    pred_pm25_rf = exp(rf_fit_pm25$testPred + rf_fit_pm25$sigma^2/2),
    lwr_pm25_rf = exp(rf_fit_pm25$interval$lower),
    upr_pm25_rf = exp(rf_fit_pm25$interval$upper)
  )

fwrite(pred_results, file = "./Data/Prediction Results.csv")
