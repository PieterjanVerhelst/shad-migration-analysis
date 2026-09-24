# Combine different behaviours in single column
# Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load libraries
library(tidyverse)
library(lubridate)
library(tidyquant)


# Load data
data_with_spawning_behavior <- read_csv('./data/interim/data_with_flag_spawning_migration.csv') 

# Replace NA by FALSE 
data_with_spawning_behavior[c("tagging_effect")][is.na(data_with_spawning_behavior[c("tagging_effect")])] <- FALSE

# Create single column 'behaviour' containing all different movement behaviour types
data_with_spawning_behavior$tagging_effect2 <- NA
for (i in 1:dim(data_with_spawning_behavior)[1]){
  if (data_with_spawning_behavior$tagging_effect[i] == TRUE){
    data_with_spawning_behavior$tagging_effect2[i] = "tagging_effect"
  }}


data_with_spawning_behavior <- data_with_spawning_behavior %>% 
  unite("behaviour", "migration","tagging_effect2", na.rm = TRUE, remove = FALSE)
data_with_spawning_behavior$behaviour[data_with_spawning_behavior$behaviour==""] <- NA
data_with_spawning_behavior <- data_with_spawning_behavior %>% 
  replace_na(list(behaviour = "foraging"))


data_with_spawning_behavior$behaviour <- factor(data_with_spawning_behavior$behaviour)

# Remove redundant columns
data_with_spawning_behavior$tagging_effect2 <- NULL
data_with_spawning_behavior$migration <- NULL


# Set behaviour in canal to foraging
data_with_spawning_behavior <-data_with_spawning_behavior %>% mutate(behaviour=replace(behaviour, location == "channel" & behaviour == 'upstream', "foraging"))

channel <- filter(data_with_spawning_behavior, location == "channel")
table(channel$behaviour)


# Set behaviour in Waddensea to foraging
data_with_spawning_behavior <-data_with_spawning_behavior %>% mutate(behaviour=replace(behaviour, location == "waddenzee" & behaviour == 'downstream', "foraging"))
data_with_spawning_behavior <-data_with_spawning_behavior %>% mutate(behaviour=replace(behaviour, location == "waddenzee" & behaviour == 'tagging_effect', "foraging"))

wadden <- filter(data_with_spawning_behavior, location == "waddenzee")
table(wadden$behaviour)

# Set behaviour in Nieuwe Waterweg to foraging
data_with_spawning_behavior <-data_with_spawning_behavior %>% mutate(behaviour=replace(behaviour, location == "nieuwe_waterweg" & behaviour == 'tagging_effect', "foraging"))

# Set behaviour in Voordelta to foraging
data_with_spawning_behavior <-data_with_spawning_behavior %>% mutate(behaviour=replace(behaviour, location == "voordelta" & behaviour == 'tagging_effect', "foraging"))

data_with_spawning_behavior <-data_with_spawning_behavior %>% mutate(behaviour=replace(behaviour, location == "voordelta" & behaviour == 'downstream', "foraging"))


# Write csv file
write.csv(data_with_spawning_behavior, "./data/interim/data_with_behaviour_types.csv")


