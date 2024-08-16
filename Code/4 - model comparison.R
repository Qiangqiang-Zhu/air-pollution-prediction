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

load("./Data/Model Comparison.RData")

cv <- rbind(cv_stat_models, cv_rf)
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
  rf_oc_no2$Pred %>% mutate(Model = "RF.oc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_lc_no2$Pred %>% mutate(Model = "RF.lc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_oc_no2$Pred %>% mutate(Model = "RF.oc.sp") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_lc_no2$Pred %>% mutate(Model = "RF.lc.sp") %>% rename(Lwr = Lower, Upr = Upper)
) %>%
  mutate(Model = factor(
    Model,
    levels = c("LM.cs", "LM.vs", "AM", "AM.sp", "AMLSS", "AMLSS.sp",
               "SP.gp", "SP.ar",
               "RF.oc", "RF.lc", "RF.oc.sp", "RF.lc.sp"),
    labels = c(expression(LM[cs]), expression(LM[vs]),
               "AM", expression(AM[sp]),
               "AMLSS", expression(AMLSS[sp]),
               expression(SP[gp]), expression(SP[ar]),
               expression(RF[oc]), expression(RF[lc]),
               expression(RF[oc_sp]), expression(RF[lc_sp]))
  ))
pred_no2$Split <- factor(pred_no2$Split)

# Figure 4 in the Supplementary Material
ggplot(pred_no2, aes(x = Obs, y = Pred, colour = Split)) +
  geom_point(size = 0.1) +
  geom_abline(col = "darkblue", linewidth = 0.3) +
  scale_color_npg() +
  labs(title = NULL, x = expression(Observation~(mu*g/m^3)), y = expression(Prediction~(mu*g/m^3))) +
  theme_bw() +
  facet_wrap(~ Model, ncol = 2, labeller = label_parsed)
ggsave(filename = "./Figure/SI_Fig4.jpeg", width = 6, height = 9, units = "in", dpi = 300)


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
  rf_oc_pm10$Pred %>% mutate(Model = "RF.oc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_lc_pm10$Pred %>% mutate(Model = "RF.lc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_oc_pm10$Pred %>% mutate(Model = "RF.oc.sp") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_lc_pm10$Pred %>% mutate(Model = "RF.lc.sp") %>% rename(Lwr = Lower, Upr = Upper)
) %>%
  mutate(Model = factor(
    Model,
    levels = c("LM.cs", "LM.vs", "AM", "AM.sp", "AMLSS", "AMLSS.sp",
               "SP.gp", "SP.ar",
               "RF.oc", "RF.lc", "RF.oc.sp", "RF.lc.sp"),
    labels = c(expression(LM[cs]), expression(LM[vs]),
               "AM", expression(AM[sp]),
               "AMLSS", expression(AMLSS[sp]),
               expression(SP[gp]), expression(SP[ar]),
               expression(RF[oc]), expression(RF[lc]),
               expression(RF[oc_sp]), expression(RF[lc_sp]))
  ))
pred_pm10$Split <- factor(pred_pm10$Split)

# Figure 5 in the Supplementary Material
ggplot(pred_pm10, aes(x = Obs, y = Pred, colour = Split)) +
  geom_point(size = 0.1) +
  geom_abline(col = "darkblue", linewidth = 0.3) +
  scale_color_npg() +
  labs(title = NULL, x = expression(Observation~(mu*g/m^3)), y = expression(Prediction~(mu*g/m^3))) +
  theme_bw() +
  facet_wrap(~ Model, ncol = 2, labeller = label_parsed)
ggsave(filename = "./Figure/SI_Fig5.jpeg", width = 6, height = 9, units = "in", dpi = 300)


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
  rf_oc_pm25$Pred %>% mutate(Model = "RF.oc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_lc_pm25$Pred %>% mutate(Model = "RF.lc") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_oc_pm25$Pred %>% mutate(Model = "RF.oc.sp") %>% rename(Lwr = Lower, Upr = Upper),
  rf_sp_lc_pm25$Pred %>% mutate(Model = "RF.lc.sp") %>% rename(Lwr = Lower, Upr = Upper)
) %>%
  mutate(Model = factor(
    Model,
    levels = c("LM.cs", "LM.vs", "AM", "AM.sp", "AMLSS", "AMLSS.sp",
               "SP.gp", "SP.ar",
               "RF.oc", "RF.lc", "RF.oc.sp", "RF.lc.sp"),
    labels = c(expression(LM[cs]), expression(LM[vs]),
               "AM", expression(AM[sp]),
               "AMLSS", expression(AMLSS[sp]),
               expression(SP[gp]), expression(SP[ar]),
               expression(RF[oc]), expression(RF[lc]),
               expression(RF[oc_sp]), expression(RF[lc_sp]))
  ))
pred_pm25$Split <- factor(pred_pm25$Split)

# Figure 6 in the Supplementary Material
ggplot(pred_pm25, aes(x = Obs, y = Pred, colour = Split)) +
  geom_point(size = 0.1) +
  geom_abline(col = "darkblue", linewidth = 0.3) +
  scale_color_npg() +
  labs(title = NULL, x = expression(Observation~(mu*g/m^3)), y = expression(Prediction~(mu*g/m^3))) +
  theme_bw() +
  facet_wrap(~ Model, ncol = 2, labeller = label_parsed)
ggsave(filename = "./Figure/SI_Fig6.jpeg", width = 6, height = 9, units = "in", dpi = 300)
