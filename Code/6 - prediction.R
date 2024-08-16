#### R script to predict monthly air pollutant concentrations across Scotland
#### using RF_log for NO2, LM_vs for PM10 and RF_log for PM2.5

library(tidyverse)
library(ranger)
library(data.table)
library(rfinterval)

source("rflog.R")

#### Data preparation ----------------------------------------------------------

data_no2 <- read.csv("./Data/Data - NO2.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas")),
    log_nearest_road_distance = log(nearest_road_distance),
    logNO2_modelled = log(NO2_modelled)
  ) %>%
  filter(!is.na(NO2))

data_pm10 <- read.csv("./Data/Data - PM10.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas")),
    log_nearest_road_distance = log(nearest_road_distance),
    logPM10_modelled = log(PM10_modelled)
  ) %>%
  filter(!is.na(PM10))

data_pm25 <- read.csv("./Data/Data - PM25.csv") %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas")),
    log_nearest_road_distance = log(nearest_road_distance),
    logPM25_modelled = log(PM25_modelled)
  ) %>%
  filter(!is.na(PM25))

pred_data <- fread("./Data/Prediction Data.csv")
pred_coords <- st_as_sf(pred_data, coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  st_coordinates()
pred_data <- pred_data %>%
  mutate(
    Type = factor(Type, levels = c("Rest", "Roadside", "Kerbside", "Industrial")),
    Year = as.factor(Year),
    Month = as.factor(Month),
    Year_Month = as.factor(Year_Month),
    UR = factor(UR, levels = c("Rural Areas", "Large Urban Areas", "Other Urban Areas")),
    log_nearest_road_distance = log(nearest_road_distance),
    logNO2_modelled = log(NO2_modelled),
    logPM10_modelled = log(PM10_modelled),
    logPM25_modelled = log(PM25_modelled),
    Longitude = pred_coords[, 1],
    Latitude = pred_coords[, 2]
  )

#### Train models --------------------------------------------------------------

## NO2

rfoc_fit_no2 <- rflog(
  formula = NO2 ~ Type + UR + Year + Month + NO2_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI +
    PopDen + log_nearest_road_distance,
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
  mutate(
    pred_no2_rfoc = rfoc_fit_no2$testPred,
    lwr_no2_rfoc = rfoc_fit_no2$interval$lower,
    upr_no2_rfoc = rfoc_fit_no2$interval$upper
  )


## PM10

lmvs_fit_pm10 <- lm(
  logPM10 ~ Type + UR + Year_Month + logPM10_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI +
    PopDen + log_nearest_road_distance,
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

rflc_fit_pm25 <- rflog(
  formula = logPM25 ~ Type + UR + Year + Month + logPM25_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI +
    PopDen + log_nearest_road_distance,
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
    pred_pm25_rflc = exp(rflc_fit_pm25$testPred + rflc_fit_pm25$sigma^2/2),
    lwr_pm25_rflc = exp(rflc_fit_pm25$interval$lower),
    upr_pm25_rflc = exp(rflc_fit_pm25$interval$upper)
  )

save.image(file = "./Data/Prediction Results.RData")
fwrite(pred_results, file = "./Data/Prediction Results.csv")
