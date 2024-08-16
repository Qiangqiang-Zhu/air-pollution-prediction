#### R script to present the prediction results

library(tidyverse)
library(sf)
library(leaflet)
library(leaflegend)
library(leafem)
library(raster)
library(data.table)
library(purrr)
library(ggsci)
library(htmltools)

pred_data <- fread("./Data/Prediction Results.csv") %>%
  mutate(Season = case_when(
    Month %in% c(3, 4, 5) ~ "Spring",
    Month %in% c(6, 7, 8) ~ "Summer",
    Month %in% c(9, 10, 11) ~ "Autumn",
    .default = "Winter"
  )) %>%
  mutate(Year = as.factor(Year), Month = as.factor(Month))

pred_data %>%
  group_by(Easting, Northing, Season) %>%
  summarise(PM25 = mean(pred_pm25_rflc), .groups = "drop") %>%
  group_by(Season) %>%
  summarise(SD = sd(PM25))

pred_data %>%
  mutate(UR = ifelse(UR == "Rural Areas", "Rural", "Urban")) %>%
  group_by(Easting, Northing, UR) %>%
  summarise(NO2 = mean(pred_no2_rfoc), .groups = "drop") %>%
  group_by(UR) %>%
  summarise(Lwr = min(NO2), Upr = max(NO2))

#### Maps of PCM modelled concentrations ---------------------------------------

pcm_sp <- pred_data %>%
  dplyr::select(Easting, Northing, NO2_modelled, PM10_modelled, PM25_modelled) %>%
  group_by(Easting, Northing) %>%
  summarise(NO2 = mean(NO2_modelled), PM10 = mean(PM10_modelled), PM25 = mean(PM25_modelled), .groups = "drop") %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_buffer(dist = 500, endCapStyle = "SQUARE") %>%
  st_transform(crs = 4326)

# Figure 3a
pal <- colorNumeric(palette = "YlOrRd", domain = pcm_sp$NO2)
leaflet(pcm_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(NO2), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~NO2, opacity = 1, title = HTML("NO<sub>2</sub> (&#181;g/m<sup>3</sup>)"))

# Figure 3b
pal <- colorNumeric(palette = "YlOrRd", domain = pcm_sp$PM10)
leaflet(pcm_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM10), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM10, opacity = 1, title = HTML("PM<sub>10</sub> (&#181;g/m<sup>3</sup>)"))

# Figure 3c
pal <- colorNumeric(palette = "YlOrRd", domain = pcm_sp$PM25)
leaflet(pcm_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM25), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM25, opacity = 1, title = HTML("PM<sub>2.5</sub> (&#181;g/m<sup>3</sup>)"))

#### Spatial plots of 5-year and seasonal mean predicted concentrations --------

pred_sp <- pred_data[, .(Easting, Northing, pred_no2_rfoc, pred_pm10_lmvs, pred_pm25_rflc)] %>%
  group_by(Easting, Northing) %>%
  summarise(NO2 = mean(pred_no2_rfoc), PM10 = mean(pred_pm10_lmvs), PM25 = mean(pred_pm25_rflc), .groups = "drop") %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_buffer(dist = 500, endCapStyle = "SQUARE") %>%
  st_transform(crs = 4326)

pred_season_sp <- pred_data[, .(Easting, Northing, Season, pred_no2_rfoc, pred_pm10_lmvs, pred_pm25_rflc)] %>%
  group_by(Easting, Northing, Season) %>%
  summarise(NO2 = mean(pred_no2_rfoc), PM10 = mean(pred_pm10_lmvs), PM25 = mean(pred_pm25_rflc), .groups = "drop") %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_buffer(dist = 500, endCapStyle = "SQUARE") %>%
  st_transform(crs = 4326)

pal <- colorNumeric(palette = "YlOrRd", domain = pred_season_sp$NO2)
leaflet(pred_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(NO2), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~NO2, opacity = 1, title = HTML("NO<sub>2</sub> (&#181;g/m<sup>3</sup>)"))
# Figures 5a-5d
leaflet(pred_season_sp %>% filter(Season == "Winter"), options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(NO2), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~NO2, opacity = 1, title = HTML("NO<sub>2</sub> (&#181;g/m<sup>3</sup>)"))


pal <- colorNumeric(palette = "YlOrRd", domain = pred_season_sp$PM10)
leaflet(pred_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM10), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM10, opacity = 1, title = HTML("PM<sub>10</sub> (&#181;g/m<sup>3</sup>)"))
# Figures 10a-10d in the supplementary material
leaflet(pred_season_sp %>% filter(Season == "Winter"), options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM10), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM10, opacity = 1, title = HTML("PM<sub>10</sub> (&#181;g/m<sup>3</sup>)"))


pal <- colorNumeric(palette = "YlOrRd", domain = pred_season_sp$PM25)
leaflet(pred_sp, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM25), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM25, opacity = 1, title = HTML("PM<sub>2.5</sub> (&#181;g/m<sup>3</sup>)"))
# Figures 6a-6d
leaflet(pred_season_sp %>% filter(Season == "Winter"), options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM25), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM25, opacity = 1, title = HTML("PM<sub>2.5</sub> (&#181;g/m<sup>3</sup>)"))


#### Figure 4: 3 line plots showing temporal trends ----------------------------

monthly_pred_data <- pred_data[, .(Year, Month, pred_no2_rfoc, pred_pm10_lmvs, pred_pm25_rflc)] %>%
  group_by(Year, Month) %>%
  summarise(
    NO2 = mean(pred_no2_rfoc),
    PM10 = mean(pred_pm10_lmvs),
    PM25 = mean(pred_pm25_rflc)
  )

axis_label <- unique(pred_data$Year_Month)
for (i in 1:15) {
  axis_label[(i - 1) * 4 + 2] <- ""
  axis_label[(i - 1) * 4 + 3] <- ""
  axis_label[(i - 1) * 4 + 4] <- ""
}

# Figure 4a
ggplot(monthly_pred_data, aes(x = Month, y = NO2, group = Year, shape = Year, colour = Year)) +
  geom_line() +
  geom_point() +
  labs(y = expression(NO[2]~(mu*g/m^3))) +
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
ggsave(filename = "./Figure/Fig4a.jpeg", width = 7, height = 5, units = "in", dpi = 300)

# Figure 4b
ggplot(monthly_pred_data, aes(x = Month, y = PM10, group = Year, shape = Year, colour = Year)) +
  geom_line() +
  geom_point() +
  labs(y = expression(PM[10]~(mu*g/m^3))) +
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
ggsave(filename = "./Figure/Fig4b.jpeg", width = 7, height = 5, units = "in", dpi = 300)

# Figure 4d
ggplot(monthly_pred_data, aes(x = Month, y = PM25, group = Year, shape = Year, colour = Year)) +
  geom_line() +
  geom_point() +
  labs(y = expression(PM[2.5]~(mu*g/m^3))) +
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
ggsave(filename = "./Figure/Fig4c.jpeg", width = 7, height = 5, units = "in", dpi = 300)


#### Figure 7 showing the yearly trends ----------------------------------------

pred_slope <- pred_data %>%
  dplyr::select(Easting, Northing, Year, pred_no2_rfoc, pred_pm10_lmvs, pred_pm25_rflc) %>%
  group_by(Easting, Northing, Year) %>%
  summarise(NO2 = mean(pred_no2_rfoc), PM10 = mean(pred_pm10_lmvs), PM25 = mean(pred_pm25_rflc), .groups = "drop") %>%
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
  st_buffer(dist = 500, endCapStyle = "SQUARE") %>%
  st_transform(crs = 4326)

# Figure 7a
pal <- colorBin(palette = c("#006837", "#66bd63", "#ffffbf", "#fdae61", "#d73027"), bins = seq(-3, 2, 1))
leaflet(pred_slope_sf, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(fit_no2), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~fit_no2, opacity = 1, title = HTML("NO<sub>2</sub> (&#181;g/m<sup>3</sup>)"))

# Figure 7b
pal <- colorBin(palette = c("#006837", "#a6d96a", "#ffffbf"), bins = seq(-1, -0.1, 0.3))
leaflet(pred_slope_sf, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(fit_pm10), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~fit_pm10, opacity = 1, title = HTML("PM<sub>10</sub> (&#181;g/m<sup>3</sup>)"))

# Figure 7c
pal <- colorBin(palette = c("#006837", "#a6d96a", "#ffffbf"), bins = seq(-0.4, 0.2, 0.2))
leaflet(pred_slope_sf, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(fit_pm25), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~fit_pm25, opacity = 1, title = HTML("PM<sub>2.5</sub> (&#181;g/m<sup>3</sup>)"))


#### Figure 8: Maps of the model uncertainty -----------------------------------

pred_interval <- pred_data %>%
  mutate(
    interval_no2 = upr_no2_rfoc - lwr_no2_rfoc,
    interval_pm10 = upr_pm10_lmvs - lwr_pm10_lmvs,
    interval_pm25 = upr_pm25_rflc - lwr_pm25_rflc
  ) %>%
  dplyr::select(Easting, Northing, interval_no2, interval_pm10, interval_pm25) %>%
  group_by(Easting, Northing) %>%
  summarise(NO2 = mean(interval_no2), PM10 = mean(interval_pm10), PM25 = mean(interval_pm25), .groups = "drop") %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_buffer(dist = 500, endCapStyle = "SQUARE") %>%
  st_transform(crs = 4326)

# Figure 8a
pal <- colorNumeric(palette = "YlOrRd", domain = pred_interval$NO2)
leaflet(pred_interval, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(NO2), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~NO2, opacity = 1, title = HTML("NO<sub>2</sub> (&#181;g/m<sup>3</sup>)"))

# Figure 8b
pal <- colorNumeric(palette = "YlOrRd", domain = pred_interval$PM10)
leaflet(pred_interval, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM10), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM10, opacity = 1, title = HTML("PM<sub>10</sub> (&#181;g/m<sup>3</sup>)"))

# Figure 8c
pal <- colorNumeric(palette = "YlOrRd", domain = pred_interval$PM25)
leaflet(pred_interval, options = leafletOptions(zoomControl = FALSE)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 50)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(fillColor = ~pal(PM25), fillOpacity = 1, color = "transparent", weight = 1, smoothFactor = 0.5) %>%
  addLegend(pal = pal, values = ~PM25, opacity = 1, title = HTML("PM<sub>2.5</sub> (&#181;g/m<sup>3</sup>)"))
