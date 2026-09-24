# Create explorative plots on the twaite shad dataset
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Packages
library(sf)
library(tmap)


# Source 
source("./src/merge_shad_characteristics.R")
source("./src/remove_false.R")

# 1. Check size - sex relationship ####
par(mfrow=c(2,1))
boxplot(length1~sex,data=shad)
boxplot(weight~sex,data=shad)
dev.off()

ggplot(shad, aes(x=factor(catch_year), y=length1)) + 
  geom_boxplot() +
  #facet_wrap(~catch_year) +
  ylab("Total length (mm)") + 
  xlab("Tagging year") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size=16)) #+
#coord_cartesian(ylim = c(300, 500))



# Select shads tagged in specific year
data <- filter(data, catch_year == 2023)


# 2. Number of shads per station ####
# Create barplot
shads_per_station <- data %>%
  group_by(catch_year, station_name, acoustic_tag_id) %>%  # Include 'catch_year' for interactive plot
  summarise(ind_shad=n_distinct(acoustic_tag_id)) %>%
  summarise(shads = n())

shads_per_station$shads <- as.numeric(shads_per_station$shads)
shads_per_station$catch_year <- factor(shads_per_station$catch_year)
#par(mar=c(6,4.1,4.1,2.1))
#barplot(shads_per_station$shads, names.arg=shads_per_station$station_name, ylim = c(0,40), cex.names=0.8, las=2)
#title(ylab="Number of shads", line = 3, cex.lab=1)
#title(xlab="Station name", line = 4, cex.lab=1)


barplot <- ggplot(data = shads_per_station, aes(x=station_name, y=shads, fill=catch_year)) +
  geom_bar(stat="identity", width = 0.75) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_brewer(palette="Spectral")
  #scale_fill_manual(values = c("Green","Blue","Red","Yellow","Purple","Orange","Brown")) 
barplot


# Create html-widget
stations <- data %>%
  select(station_name, deploy_longitude, deploy_latitude) %>%
  unique()

shads_per_station <- left_join(shads_per_station, stations, by= "station_name")

# Set interactive view
ttm() # switches between static plot and interactive viewing

# Create sf
shads_per_station$shads <- factor(shads_per_station$shads)
spatial <- st_as_sf(shads_per_station,
                             coords = c(4:5),
                             crs = 4326)  # WGS84

# Create interactive map
shads_per_station_map <- tm_shape(spatial) + 
  tm_dots(col = "shads", id = "station_name", palette = "Spectral", size = 0.5) +
  tm_facets(by = "catch_year",  ncol = 2, nrow = 3, free.scales = FALSE) +
  tmap_options(limits = c(facets.view = 6), max.categories = 10) 
shads_per_station_map


# 3. Number of stations per shad ####
stations_per_shad <- data %>%
  group_by(acoustic_tag_id, station_name) %>%
  dplyr::select(acoustic_tag_id, station_name) %>%
  summarise(ind_station=n_distinct(station_name)) %>%
  summarise(stations = n())
stations_per_shad$stations=as.numeric(stations_per_shad$stations)

par(mar=c(7,4.1,4.1,2.1))
barplot(stations_per_shad$stations, names.arg=stations_per_shad$acoustic_tag_id, ylim = c(0,100), cex.names=0.8, las=2)
title(ylab="Number of stations", line = 3, cex.lab=1)
title(xlab="Transmitter ID", line = 6, cex.lab=1)


# 4. Calculate tracking period in days ####
# Select alive shads
alive_shad <- filter(stations_per_shad, stations > 5)
data_alive <- data %>%
  filter(acoustic_tag_id %in% alive_shad$acoustic_tag_id)

period <- data_alive %>%
    group_by(acoustic_tag_id)%>%
    dplyr::select(acoustic_tag_id, date_time) %>%
    summarise(min_date = min(date_time),
              max_date = max(date_time))
period$days <- round(difftime(period$max_date, period$min_date, "days"), 1)
period$days <- as.numeric(period$days)

par(mar=c(8,6.1,2.1,1.1))
barplot(period$days, names.arg=period$acoustic_tag_id, cex.names=0.8, ylim=c(0,1.3*max(period$days)),las=2)
title(ylab="Tracking days", line = 4, cex.lab=1)
title(xlab="Transmitter ID", line = 6, cex.lab=1) 
abline(h=6, col = "red", lwd=2)
  