library(tidyverse)
library(data.table)
library(viridis)
library(ranger)
library(vip)

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2, na.rm = TRUE))
mae <- function(y, yhat) median(abs(y - yhat), na.rm = TRUE)
bias <- function(y, yhat) mean(yhat - y)
coverage <- function(y, lwr, upr) mean(y >= lwr & y <= upr)
aiw <- function(lwr, upr) mean(abs(upr - lwr))

#### Model performance varying across months

load("./Data/Model Comparison.RData")

# Combining prediction data
pred_no2 <- rbind(
  lm_cs_no2$Pred %>% mutate(Model = "LM.cs"),
  lm_vs_no2$Pred %>% mutate(Model = "LM.vs"),
  am_no2$Pred %>% mutate(Model = "AM"),
  am_sp_no2$Pred %>% mutate(Model = "AM.sp"),
  amlss_no2$Pred %>% mutate(Model = "AMLSS"),
  amlss_sp_no2$Pred %>% mutate(Model = "AMLSS.sp"),
  sp_gp_no2$Pred %>% mutate(Model = "SP.gp"),
  sp_ar_no2$Pred %>% mutate(Model = "SP.ar"),
  rf_oc_no2$Pred %>% mutate(Model = "RF.oc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_lc_no2$Pred %>% mutate(Model = "RF.lc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_oc_no2$Pred %>% mutate(Model = "RF.oc.sp") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_lc_no2$Pred %>% mutate(Model = "RF.lc.sp") %>% rename(Lwr = Lower, Upr = Upper)
) %>%
  group_by(Year_Month, Model) %>%
  summarise(RMSE = rmse(Obs, Pred), .groups = "drop") %>%
  mutate(Pollutant = "NO2")

pred_pm10 <- rbind(
  lm_cs_pm10$Pred %>% mutate(Model = "LM.cs"),
  lm_vs_pm10$Pred %>% mutate(Model = "LM.vs"),
  am_pm10$Pred %>% mutate(Model = "AM"),
  am_sp_pm10$Pred %>% mutate(Model = "AM.sp"),
  amlss_pm10$Pred %>% mutate(Model = "AMLSS"),
  amlss_sp_pm10$Pred %>% mutate(Model = "AMLSS.sp"),
  sp_gp_pm10$Pred %>% mutate(Model = "SP.gp"),
  sp_ar_pm10$Pred %>% mutate(Model = "SP.ar"),
  rf_oc_pm10$Pred %>% mutate(Model = "RF.oc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_lc_pm10$Pred %>% mutate(Model = "RF.lc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_oc_pm10$Pred %>% mutate(Model = "RF.oc.sp") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_lc_pm10$Pred %>% mutate(Model = "RF.lc.sp") %>% rename(Lwr = Lower, Upr = Upper)
) %>%
  group_by(Year_Month, Model) %>%
  summarise(RMSE = rmse(Obs, Pred), .groups = "drop") %>%
  mutate(Pollutant = "PM10")

pred_pm25 <- rbind(
  lm_cs_pm25$Pred %>% mutate(Model = "LM.cs"),
  lm_vs_pm25$Pred %>% mutate(Model = "LM.vs"),
  am_pm25$Pred %>% mutate(Model = "AM"),
  am_sp_pm25$Pred %>% mutate(Model = "AM.sp"),
  amlss_pm25$Pred %>% mutate(Model = "AMLSS"),
  amlss_sp_pm25$Pred %>% mutate(Model = "AMLSS.sp"),
  sp_gp_pm25$Pred %>% mutate(Model = "SP.gp"),
  sp_ar_pm25$Pred %>% mutate(Model = "SP.ar"),
  rf_oc_pm25$Pred %>% mutate(Model = "RF.oc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_lc_pm25$Pred %>% mutate(Model = "RF.lc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_oc_pm25$Pred %>% mutate(Model = "RF.oc.sp") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_lc_pm25$Pred %>% mutate(Model = "RF.lc.sp") %>% rename(Lwr = Lower, Upr = Upper)
) %>%
  group_by(Year_Month, Model) %>%
  summarise(RMSE = rmse(Obs, Pred), .groups = "drop") %>%
  mutate(Pollutant = "PM25")

pred <- rbind(pred_no2, pred_pm10, pred_pm25) %>%
  mutate(Pollutant = factor(
    Pollutant, levels = c("NO2", "PM10", "PM25"),
    labels = c(expression(NO[2]), expression(PM[10]), expression(PM[2.5]))
  ))

# Axis labels
axis_label <- unique(as.character(pred_no2$Year_Month))
for (i in 1:length(axis_label)) {
  if ((i - 1) %% 4 != 0) {
    axis_label[i] <- ""
  }
}

# Define model names as expressions
model_names <- c(
  "LM.cs" = expression(LM[cs]),
  "LM.vs" = expression(LM[vs]),
  "AM" = "AM",
  "AM.sp" = expression(AM[sp]),
  "AMLSS" = "AMLSS",
  "AMLSS.sp" = expression(AMLSS[sp]),
  "SP.gp" = expression(SP[gp]),
  "SP.ar" = expression(SP[ar]),
  "RF.oc" = expression(RF[oc]),
  "RF.lc" = expression(RF[lc]),
  "RF.oc.sp" = expression(RF[oc.sp]),
  "RF.lc.sp" = expression(RF[lc.sp])
)

# Figure 7 in the supplementary material
ggplot(data = pred, aes(x = Year_Month, y = RMSE, group = Model, colour = Model)) +
  geom_line() +
  scale_color_viridis_d(labels = model_names) +
  scale_x_discrete(labels = axis_label) +
  labs(title = NULL, x = NULL, y = "RMSE") +
  theme_bw() +
  facet_grid(rows = vars(Pollutant), scales = "free", labeller = label_parsed) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
ggsave(filename = "./Figure/SI_Fig7.jpeg", width = 7, height = 8, units = "in", dpi = 300)

#### Model structure of best performing models

rm(list = ls())

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

rf_no2 <- ranger(
  NO2 ~ Type + UR + Year + Month + NO2_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI +
    PopDen + log_nearest_road_distance,
  data = data_no2,
  num.trees = 500,
  mtry = 4,
  min.bucket = 3,
  max.depth = 10,
  verbose = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  seed = 123
)

axis_label <- c(
  "Type" = "Site type",
  "UR" = "Urban Rural Classification",
  "Year" = "Year",
  "Month" = "Month",
  "NO2_modelled" = "PCM estimates",
  "Hurs" = "Humidity",
  "Psl" = "Sea level pressure",
  "SfcWind" = "Wind speed",
  "Sun" = "Sunshine hours",
  "Rainfall" = "Rainfall",
  "Tas" = "Temperature",
  "NDVI" = "NDVI",
  "PopDen" = "Population density",
  "log_nearest_road_distance" = "Distance to the nearest road"
)

# Figure 8 in the supplementary material
vip(rf_no2, num_features = 15) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  xlab("Variables") +
  theme_minimal() +
  scale_x_discrete(labels = axis_label) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
ggsave(filename = "./Figure/SI_Fig8.jpeg", width = 7, height = 5, units = "in", dpi = 300)

normalise <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

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
  filter(!is.na(PM10)) %>%
  mutate_if(is.numeric, normalise)

lm_pm10 <- lm(
  logPM10 ~ Type + UR + Year_Month + logPM10_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI +
    PopDen + log_nearest_road_distance,
  data = data_pm10
)
# Table 3 in the supplementary material
summary(lm_pm10)

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

rf_pm25 <- ranger(
  logPM25 ~ Type + UR + Year + Month + logPM25_modelled +
    Hurs + Psl + SfcWind + Sun + Rainfall + Tas + NDVI +
    PopDen + log_nearest_road_distance,
  data = data_pm25,
  num.trees = 500,
  mtry = 8,
  min.bucket = 3,
  max.depth = 20,
  verbose = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  seed = 123
)

axis_label <- c(
  "Type" = "Site type",
  "UR" = "Urban Rural Classification",
  "Year" = "Year",
  "Month" = "Month",
  "logPM25_modelled" = "PCM estimates",
  "Hurs" = "Humidity",
  "Psl" = "Sea level pressure",
  "SfcWind" = "Wind speed",
  "Sun" = "Sunshine hours",
  "Rainfall" = "Rainfall",
  "Tas" = "Temperature",
  "NDVI" = "NDVI",
  "PopDen" = "Population density",
  "log_nearest_road_distance" = "Distance to the nearest road"
)

# Figure 9 in the supplementary material
vip(rf_pm25, num_features = 15) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  xlab("Variables") +
  theme_minimal() +
  scale_x_discrete(labels = axis_label) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
ggsave(filename = "./Figure/SI_Fig9.jpeg", width = 7, height = 5, units = "in", dpi = 300)
