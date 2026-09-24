# Add shad release positions and date-time to detection dataset
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)


# 1. Read shad meta data
shad <- read_csv("./data/raw/shad_meta_data.csv")

shad <- dplyr::select(shad, 
              animal_project_code, 
              scientific_name, 
              release_date_time,
              tag_serial_number,
              acoustic_tag_id, 
              release_location, 
              release_latitude, 
              release_longitude)


shad$release_location <- factor(shad$release_location)


# 2. Read file with release location and station
release <- read_csv("./data/external/release_locations_stations.csv")

release$release_location <- factor(release$release_location)
release$release_station <- factor(release$release_station)

# 4. Merge release station with shad data
shad <- left_join(shad, release, by = "release_location")

# 5. Process shad dataset column names
shad$receiver_id <- "none"
shad$network_project_code <- "zeeschelde"

shad <- dplyr::select(shad,
              animal_project_code, 
              scientific_name, 
              release_date_time,
              tag_serial_number,
              acoustic_tag_id, 
              release_station,
              network_project_code,
              receiver_id,
              release_latitude, 
              release_longitude)

shad <- rename(shad,
              date_time = release_date_time,
              station_name = release_station,
              acoustic_project_code = network_project_code,
              deploy_latitude = release_latitude,
              deploy_longitude = release_longitude)

# 6. Merge shad releases to the detection dataset
data <- read_csv("./data/raw/raw_detection_data.csv")
data$...1 <- NULL

# Set consistent prefix for tag protocol
data$acoustic_tag_id <- gsub("R64K", "A69-1303", data$acoustic_tag_id)
shad$acoustic_tag_id <- gsub("R64K", "A69-1303", shad$acoustic_tag_id)


data <- rbind(data, shad)




