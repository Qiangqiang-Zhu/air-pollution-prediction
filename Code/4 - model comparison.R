#### R script for model comparison

library(tidyverse)
library(data.table)
library(ggpubr)
library(ggsci)
library(GGally)
library(ggrepel)
library(RColorBrewer)
library(cowplot)
library(patchwork)

load("./Data/model comparison.RData")

cv <- rbind(lm_cv, am_cv, am_sp_cv, amlss_cv, amlss_sp_cv, sp_gp_cv, sp_ar_cv, cv_rf)
cv  # Table 1

## Plots of observed v.s. predicted values -------------------------------------

### NO2

pred_no2 <- rbind(
  lm_cs_no2$Pred %>% mutate(Model = "LM.cs"),
  lm_vs_no2$Pred %>% mutate(Model = "LM.vs"),
  am_no2$Pred %>% mutate(Model = "AM"),
  am_sp_no2$Pred %>% mutate(Model = "AM.sp"),
  amlss_no2$Pred %>% mutate(Model = "AMLSS"),
  amlss_sp_no2$Pred %>% mutate(Model = "AMLSS.sp"),
  sp_gp_no2$Pred %>% mutate(Model = "SP.gp"),
  sp_ar_no2$Pred %>% mutate(Model = "SP.ar"),
  rf_no2$Pred %>% mutate(Model = "RF.oc"),
  rflog_no2$Pred %>% mutate(Model = "RF.lc")
) %>%
  mutate(Model = factor(
    Model,
    levels = c("LM.cs", "LM.vs",
               "AM", "AM.sp",
               "AMLSS", "AMLSS.sp",
               "SP.gp", "SP.ar",
               "RF.oc", "RF.lc"),
    labels = c(expression(LM[cs]), expression(LM[vs]),
               "AM", expression(AM[sp]),
               "AMLSS", expression(AMLSS[sp]),
               expression(SP[gp]), expression(SP[ar]),
               expression(RF[oc]), expression(RF[lc]))
  ))
pred_no2$Split <- factor(pred_no2$Split)

# Figure 4 in the Supplementary Material
ggplot(pred_no2, aes(x = Obs, y = Pred, colour = Split)) +
  geom_point(size = 0.1) +
  geom_abline(col = "darkblue", linewidth = 0.3) +
  scale_color_npg() +
  theme_bw() +
  facet_wrap(~ Model, ncol = 2, labeller = label_parsed)


### PM10

pred_pm10 <- rbind(
  lm_cs_pm10$Pred %>% mutate(Model = "LM.cs"),
  lm_vs_pm10$Pred %>% mutate(Model = "LM.vs"),
  am_pm10$Pred %>% mutate(Model = "AM"),
  am_sp_pm10$Pred %>% mutate(Model = "AM.sp"),
  amlss_pm10$Pred %>% mutate(Model = "AMLSS"),
  amlss_sp_pm10$Pred %>% mutate(Model = "AMLSS.sp"),
  sp_gp_pm10$Pred %>% mutate(Model = "SP.gp"),
  sp_ar_pm10$Pred %>% mutate(Model = "SP.ar"),
  rf_pm10$Pred %>% mutate(Model = "RF.oc"),
  rflog_pm10$Pred %>% mutate(Model = "RF.lc")
) %>%
  mutate(Model = factor(
    Model,
    levels = c("LM.cs", "LM.vs",
               "AM", "AM.sp",
               "AMLSS", "AMLSS.sp",
               "SP.gp", "SP.ar",
               "RF.oc", "RF.lc"),
    labels = c(expression(LM[cs]), expression(LM[vs]),
               "AM", expression(AM[sp]),
               "AMLSS", expression(AMLSS[sp]),
               expression(SP[gp]), expression(SP[ar]),
               expression(RF[oc]), expression(RF[lc]))
  ))
pred_pm10$Split <- factor(pred_pm10$Split)

# Figure 5 in the Supplementary Material
ggplot(pred_pm10, aes(x = Obs, y = Pred, colour = Split)) +
  geom_point(size = 0.1) +
  geom_abline(col = "darkblue", linewidth = 0.3) +
  scale_color_npg() +
  theme_bw() +
  facet_wrap(~ Model, ncol = 2, labeller = label_parsed)


### PM25

pred_pm25 <- rbind(
  lm_cs_pm25$Pred %>% mutate(Model = "LM.cs"),
  lm_vs_pm25$Pred %>% mutate(Model = "LM.vs"),
  am_pm25$Pred %>% mutate(Model = "AM"),
  am_sp_pm25$Pred %>% mutate(Model = "AM.sp"),
  amlss_pm25$Pred %>% mutate(Model = "AMLSS"),
  amlss_sp_pm25$Pred %>% mutate(Model = "AMLSS.sp"),
  sp_gp_pm25$Pred %>% mutate(Model = "SP.gp"),
  sp_ar_pm25$Pred %>% mutate(Model = "SP.ar"),
  rf_pm25$Pred %>% mutate(Model = "RF.oc"),
  rflog_pm25$Pred %>% mutate(Model = "RF.lc")
) %>%
  mutate(Model = factor(
    Model,
    levels = c("LM.cs", "LM.vs",
               "AM", "AM.sp",
               "AMLSS", "AMLSS.sp",
               "SP.gp", "SP.ar",
               "RF.oc", "RF.lc"),
    labels = c(expression(LM[cs]), expression(LM[vs]),
               "AM", expression(AM[sp]),
               "AMLSS", expression(AMLSS[sp]),
               expression(SP[gp]), expression(SP[ar]),
               expression(RF[oc]), expression(RF[lc]))
  ))
pred_pm25$Split <- factor(pred_pm25$Split)

# Figure 6 in the Supplementary Material
ggplot(pred_pm25, aes(x = Obs, y = Pred, colour = Split)) +
  geom_point(size = 0.1) +
  geom_abline(col = "darkblue", linewidth = 0.3) +
  scale_color_npg() +
  theme_bw() +
  facet_wrap(~ Model, ncol = 2, labeller = label_parsed)
