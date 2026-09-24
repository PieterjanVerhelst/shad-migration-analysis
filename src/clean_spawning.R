# Clean spawning behaviour data by selecting data from the estuary and remove seaward movement events in a spawning event
# Pieterjan Verhelst
# pieterjan.verhelst@inbo.be

# Load libraries
library(tidyverse)
library(lubridate)


# Load data
data <- read_csv('./data/interim/data_with_behaviour_types.csv') 

# Filter spawning event
data <- filter(data, behaviour == "upstream" |
                 behaviour == "spawning" |
                 behaviour == "downstream")

# Select estuarine detections
data <- filter(data, location != "bpns")
