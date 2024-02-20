#### R script for exploratory data analysis

library(tidyverse)
library(leaflet)
library(sf)
library(sp)
library(lubridate)
library(ggTimeSeries)
library(scales)

monitor_sites <- read.csv("./Data/Scotland air pollution monitor locations.csv")

sites_info <- monitor_sites %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame()
colnames(sites_info) <- c("Longitude", "Latitude")
sites_info <- cbind(monitor_sites, sites_info)

# Figure 1
pal <- colorFactor(c("darkblue", "darkgreen", "darkorchid", "cadetblue", "darkorange", "firebrick"),
                   domain = unique(sites_info$type))
leaflet(sites_info, options = leafletOptions(zoomControl = FALSE)) %>%
  setView(lng = -3.6, lat = 58, zoom = 6.2) %>%
  addTiles() %>%
  addCircleMarkers(radius = 2, color = ~pal(Type), stroke = FALSE, fillOpacity = 1) %>%
  addLegend(position = "topright", pal = pal, values = ~Type, title = "Type of Monitoring Sites", opacity = 1)

data <- read.csv("./Data/Monitoring Data.csv") %>% left_join(sites_info, by = c("Site" = "Name"))

# Table 1 in the Supplementary Material
summary(data[, 3:5])

data[which(data$NO2 < 0), "NO2"] <- NA
data[which(data$PM10 < 0), "PM10"] <- NA
data[which(data$PM25 < 0), "PM25"] <- NA

plot_missing <- function(pollutant_name) {
  if (pollutant_name == "NO2") {
    fill_name <- expression(NO[2])
  } else if (pollutant_name == "PM10") {
    fill_name <- expression(PM[10])
  } else if (pollutant_name == "PM25") {
    fill_name <- expression(PM[2.5])
  } else {
    stop("Error: The name of pollutant is wrong!")
  }

  data_new <- data %>%
    filter(Date == "2020-12-31") %>%
    mutate(Date = as.Date("2021-01-01")) %>%
    rbind(data) %>%
    dplyr::select(Date, Site, pollutant_name) %>%
    arrange(Site, Date)

  data_new %>%
    spread(Site, pollutant_name) %>%
    dplyr::select(-Date) %>%
    is.na() %>%
    reshape2::melt() %>%
    mutate(Site = factor(Var2, levels = rev(unique(data$Site))),
           Date = data_new$Date) %>%
    ggplot(aes(Site, Date, fill = value)) +
    geom_raster() +
    coord_flip() +
    scale_y_date(expand = c(0, 0), breaks = date_breaks("1 year"),
                 labels = date_format("%Y")) +
    scale_fill_grey(name = fill_name, labels = c("Present", "Missing")) +
    labs(x = "Monitoring Sites", y = "Date") +
    theme(
      axis.text.y = element_text(size = 4),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 12)
    )
}

# Figures 1-3 in the Supplementary Material
plot_missing("NO2")
plot_missing("PM10")
plot_missing("PM25")

data$Year <- as.factor(year(data$Date))
data$Month <- as.factor(month(data$Date))
data$Year_Month <- as.factor(substr(data$Date, 1, 7))

axis_label <- unique(as.character(data$Year_Month))
for (i in 1:15) {
  axis_label[(i - 1) * 4 + 2] <- ""
  axis_label[(i - 1) * 4 + 3] <- ""
  axis_label[(i - 1) * 4 + 4] <- ""
}

monthly_data <- data %>%
  group_by(Year, Month, Year_Month, Site) %>%
  summarise(
    NO2 = mean(NO2, na.rm = TRUE),
    PM10 = mean(PM10, na.rm = TRUE),
    PM25 = mean(PM25, na.rm = TRUE)
  )

# Figure 2
ggplot(data = monthly_data, aes(x = Year_Month, y = NO2)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, show.legend = FALSE, colour = "cadetblue") +
  scale_x_discrete(labels = axis_label) +
  labs(title = NULL, x = NULL, y = expression(NO[2])) +
  stat_summary(fun = median, geom = "line", aes(group = 1), colour = "darkblue") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )
ggplot(data = monthly_data, aes(x = Year_Month, y = PM10)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, show.legend = FALSE, colour = "cadetblue") +
  scale_x_discrete(labels = axis_label) +
  labs(title = NULL, x = NULL, y = expression(PM[10])) +
  stat_summary(fun = median, geom = "line", aes(group = 1), colour = "darkblue") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )
ggplot(data = monthly_data, aes(x = Year_Month, y = PM25)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, show.legend = FALSE, colour = "cadetblue") +
  scale_x_discrete(labels = axis_label) +
  labs(title = NULL, x = NULL, y = expression(PM[2.5])) +
  stat_summary(fun = median, geom = "line", aes(group = 1), colour = "darkblue") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20)
  )
