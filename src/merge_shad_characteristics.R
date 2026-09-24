# Merge shad meta data to tracking dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@inbo.be

library(tidyverse)
library(lubridate)



# 1. Source ####
source("./src/attach_release.R")

data$tag_serial_number <- factor(data$tag_serial_number)
data$acoustic_tag_id <- factor(data$acoustic_tag_id)
data$date_time  <- as_datetime(data$date_time)


# 2. Load shad metadata ####
shad <- read.csv("./data/raw/shad_meta_data.csv")
shad$X <- NULL
shad$animal_project_code <- NULL
shad$scientific_name <- NULL
shad$age <- NULL
shad$age_unit <- NULL
shad$life_stage <- NULL
shad$acoustic_tag_id <- NULL
shad$tag_serial_number <- factor(shad$tag_serial_number)
#shad$acoustic_tag_id <- factor(shad$acoustic_tag_id)
#shad$acoustic_tag_id <- gsub("R64K", "A69-1303", shad$acoustic_tag_id)   # Set consistent prefix for tag protocol
shad$capture_date_time  <- as_datetime(shad$capture_date_time)
shad$release_date_time  <- as_datetime(shad$release_date_time)

# Return number of tagged shads per year ####
shad$catch_year <- year(shad$capture_date_time)

shad %>%
  group_by(catch_year) %>%
  summarise(tot_shads = n_distinct(tag_serial_number))


# 3. Merge shad characteristics with dataset ####
data <- merge(data, shad, by="tag_serial_number")


# Return number of detected shads per year ####
data %>%
  group_by(catch_year) %>%
  summarise(tot_shads = n_distinct(tag_serial_number))


