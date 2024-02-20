#### R script to present the prediction results

library(tidyverse)
library(sf)
library(leaflet)
library(leafem)
library(raster)
library(data.table)
library(purrr)
library(ggsci)

pred_data <- fread("./Data/Prediction Results.csv") %>%
  mutate(Season = case_when(
    Month %in% c(3, 4, 5) ~ "Spring",
    Month %in% c(6, 7, 8) ~ "Summer",
    Month %in% c(9, 10, 11) ~ "Autumn",
    .default = "Winter"
  )) %>%
  mutate(Year = as.factor(Year), Month = as.factor(Month))

# 3 maps by averaging concentrations across 60 months

pred_sp <- pred_data %>%
  dplyr::select(Easting, Northing, pred_no2_rf, pred_pm10_lmvs, pred_pm25_rf) %>%
  group_by(Easting, Northing) %>%
  summarise(NO2 = mean(pred_no2_rf), PM10 = mean(pred_pm10_lmvs), PM25 = mean(pred_pm25_rf)) %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(crs = 4326)

pred_season_sp <- pred_data %>%
  dplyr::select(Easting, Northing, Season, pred_no2_rf, pred_pm10_lmvs, pred_pm25_rf) %>%
  group_by(Easting, Northing, Season) %>%
  summarise(NO2 = mean(pred_no2_rf), PM10 = mean(pred_pm10_lmvs), PM25 = mean(pred_pm25_rf)) %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(crs = 4326)

# Spatial plots of 5-year and seasonal mean predicted concentrations

pal <- colorNumeric(palette = "YlOrRd", domain = pred_season_sp$NO2)
leaflet(pred_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(NO2), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~NO2, opacity = 1, title = "NO2")
leaflet(pred_season_sp %>% filter(Season == "Winter"), options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(NO2), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~NO2, opacity = 1, title = "NO2")


pal <- colorNumeric(palette = "YlOrRd", domain = pred_season_sp$PM10)
leaflet(pred_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(PM10), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~PM10, opacity = 1, title = "PM10")
leaflet(pred_season_sp %>% filter(Season == "Winter"), options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(PM10), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~PM10, opacity = 1, title = "PM10")


pal <- colorNumeric(palette = "YlOrRd", domain = pred_season_sp$PM25)
leaflet(pred_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(PM25), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~PM25, opacity = 1, title = "PM2.5")
leaflet(pred_season_sp %>% filter(Season == "Winter"), options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(PM25), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~PM25, opacity = 1, title = "PM2.5")


# Figure 6: 3 box plots showing temporal trends

monthly_pred_data <- pred_data[, .(Year, Month, pred_no2_rf, pred_pm10_lmvs, pred_pm25_rf)] %>%
  group_by(Year, Month) %>%
  summarise(
    NO2 = mean(pred_no2_rf),
    PM10 = mean(pred_pm10_lmvs),
    PM25 = mean(pred_pm25_rf)
  )

axis_label <- unique(pred_data$Year_Month)
for (i in 1:15) {
  axis_label[(i - 1) * 4 + 2] <- ""
  axis_label[(i - 1) * 4 + 3] <- ""
  axis_label[(i - 1) * 4 + 4] <- ""
}

ggplot(monthly_pred_data, aes(x = Month, y = NO2, group = Year, shape = Year, colour = Year)) +
  geom_line() +
  geom_point() +
  labs(y = expression(NO[2])) +
  scale_color_npg() +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  )

ggplot(monthly_pred_data, aes(x = Month, y = PM10, group = Year, shape = Year, colour = Year)) +
  geom_line() +
  geom_point() +
  labs(y = expression(PM[10])) +
  scale_color_npg() +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  )

ggplot(monthly_pred_data, aes(x = Month, y = PM25, group = Year, shape = Year, colour = Year)) +
  geom_line() +
  geom_point() +
  labs(y = expression(PM[2.5])) +
  scale_color_npg() +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10)
  )


# Figure 7

pred_slope <- pred_data %>%
  dplyr::select(Easting, Northing, Year, pred_no2_rf, pred_pm10_lmvs, pred_pm25_rf) %>%
  group_by(Easting, Northing, Year) %>%
  summarise(NO2 = mean(pred_no2_rf), PM10 = mean(pred_pm10_lmvs), PM25 = mean(pred_pm25_rf)) %>%
  mutate(Year = as.numeric(Year) - 2015) %>%
  nest(data = -c(Easting, Northing)) %>%
  mutate(
    fit_no2 = map(data, ~coef(lm(NO2 ~ Year, data = .))[[2]]),
    fit_pm10 = map(data, ~coef(lm(PM10 ~ Year, data = .))[[2]]),
    fit_pm25 = map(data, ~coef(lm(PM25 ~ Year, data = .))[[2]])
  ) %>%
  dplyr::select(-data) %>%
  unnest(c(fit_no2, fit_pm10, fit_pm25))

pred_slope_sf <- pred_slope %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(crs = 4326)

pal <- colorBin(palette = c("#006837", "#66bd63", "#ffffbf", "#fdae61", "#d73027"), bins = seq(-3, 2, 1))
leaflet(pred_slope_sf, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(fit_no2), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~fit_no2, opacity = 1, title = "NO2")

pal <- colorBin(palette = c("#006837", "#a6d96a", "#ffffbf"), bins = seq(-1.1, -0.2, 0.3))
leaflet(pred_slope_sf, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(fit_pm10), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~fit_pm10, opacity = 1, title = "PM10")

pal <- colorBin(palette = c("#006837", "#a6d96a", "#ffffbf", "#fdae61"), bins = seq(-0.4, 0.4, 0.2))
leaflet(pred_slope_sf, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(fit_pm25), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~fit_pm25, opacity = 1, title = "PM2.5")


# Figure 8: 3 maps of uncertainty

pred_interval <- pred_data %>%
  mutate(
    interval_no2 = upr_no2_rf - lwr_no2_rf,
    interval_pm10 = upr_pm10_lmvs - lwr_pm10_lmvs,
    interval_pm25 = upr_pm25_rf - lwr_pm25_rf
  ) %>%
  dplyr::select(Easting, Northing, interval_no2, interval_pm10, interval_pm25) %>%
  group_by(Easting, Northing) %>%
  summarise(NO2 = mean(interval_no2), PM10 = mean(interval_pm10), PM25 = mean(interval_pm25)) %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(crs = 4326)

pal <- colorNumeric(palette = "YlOrRd", domain = pred_interval$NO2)
leaflet(pred_interval, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(NO2), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~NO2, opacity = 1, title = "NO2")

pal <- colorNumeric(palette = "YlOrRd", domain = pred_interval$PM10)
leaflet(pred_interval, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(PM10), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~PM10, opacity = 1, title = "PM10")

pal <- colorNumeric(palette = "YlOrRd", domain = pred_interval$PM25)
leaflet(pred_interval, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircles(fillColor = ~pal(PM25), fillOpacity = 1, radius = 500, stroke = FALSE) %>%
  addLegend(pal = pal, values = ~PM25, opacity = 1, title = "PM2.5")
