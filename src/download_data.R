# Download data from shad study from ETN database via RStudio LifeWatch server
# by Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Install and load packages
install.packages("devtools") 
install.packages("digest")
#install.packages("processx")
devtools::install_github("inbo/etn")

library(tidyverse)
library(lubridate)
library(etn)


# Database connection
my_con <- connect_to_etn("pieterjan.verhelst@inbo.be",  # login - email address
                         "XXXX")   # password
my_con



# 1. Download detection data ####
data <- get_acoustic_detections(my_con, scientific_name = "Alosa fallax",
                                animal_project_code = "2015_fint",
                                limit = FALSE)

# Select relevant columns
data <- select(data, animal_project_code, scientific_name, date_time, tag_serial_number, acoustic_tag_id, station_name, acoustic_project_code, receiver_id, deploy_latitude, deploy_longitude)

# Remove detections at station ak-x
data <- data[!(data$station_name=="ak-x"),]


# 2. Download shad meta-data ####
shads <- get_animals(my_con, scientific_name = "Alosa fallax", 
                     animal_project_code = "2015_fint")

# Select relevant columns
shads <- select(shads, animal_project_code, scientific_name,  tag_serial_number, acoustic_tag_id,
                capture_date_time, capture_location, capture_latitude, capture_longitude, capture_method,
                release_date_time, release_location, release_latitude, release_longitude,
                length1_type, length1, length1_unit,
                weight, weight_unit,
                age, age_unit, sex, life_stage, 
                tagging_type)


# 3. Download deployment positions ####
network_projects <- c("ws2",
                      "ws3",
                      "zeeschelde",
                      "ws1",
                      "bpns",
                      "cpodnetwork",
                      "FISHINTEL",
                      "Walloneel",
                      "Haringvliet2023-2026",
                      "dijle",
                      "FISP",
                      "PelFish",
                      "SWIMWAY_2021",
                      "Grotenete",
                      "FISHOWF",
                      "2024_bovenschelde",
                      "PureWind_fish_detectors",
                      "albert",
                      "leopold",
                      "Apelafico",
                      "Orstedcod",
                      "mrc_vliz",
                      "CODEVCO_fish_detectors") 


deployments <- get_acoustic_deployments(my_con, acoustic_project_code = network_projects, open_only = FALSE)
deployments$station_name <- factor(deployments$station_name)

# Get unique deployments
#unique_deployments <- deployments %>% 
#  group_by(acoustic_project_code) %>%
#  distinct(station_name, .keep_all = TRUE) 

# Select relevant columns
deployments <- select(deployments, acoustic_project_code, station_name, receiver_id, deploy_latitude, deploy_longitude, deploy_date_time, recover_date_time)


# 4. Check transmitter specs ####
tags_id <- list_values(shads, tag_serial_number)
tags_alosa <- get_tags(my_con, tag_serial_number = tags_id)
tags_alosa <- select(tags_alosa, tag_serial_number, acoustic_tag_id, acoustic_tag_id_alternative, manufacturer, model, activation_date, battery_estimated_life, battery_estimated_end_date)



# write csv
write.csv(data, "raw_detection_data.csv")
write.csv(shads, "shad_meta_data.csv")
write.csv(deployments, "deployments.csv")
write.csv(tags_alosa, "tag_specifications.csv")

