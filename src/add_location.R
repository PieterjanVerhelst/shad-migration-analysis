# Add detection location to the station names
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Source 
source("./src/merge_shad_characteristics2.R")
data$station_name <- factor(data$station_name)

# Load deployments
deployments <- read_csv('./data/raw/deployments.csv') 
deployments <- deployments %>%
  select(station_name, acoustic_project_code) %>%
  distinct()
deployments <- na.omit(deployments)

# Add release stations to deployments
release <- data.frame(station_name  = c("rel_station1", "rel_station2","rel_station3","rel_station4"),
                  acoustic_project_code = c("zeeschelde", "zeeschelde","zeeschelde","zeeschelde")
)

deployments <- rbind(deployments, release)
rm(release)

deployments$station_name <- factor(deployments$station_name)
deployments$acoustic_project_code <- factor(deployments$acoustic_project_code)

# Set locations
table(deployments$acoustic_project_code)
deployments$acoustic_project_code <- recode_factor(deployments$acoustic_project_code, 
                                                   Apelafico = "bpns",
                                                   cpodnetwork = "bpns",
                                                   leopold = "ws2",
                                                   albert = "zeeschelde",
                                                   SWIMWAY_2021 = "waddenzee",
                                                   dijle = "rupel",           # Check this is still correct at next data upload
                                                   demer = "demer",
                                                   Danish_Straits = "danish_straits",
                                                   FISHOWF = "bay_of_biscay",
                                                   mrc_vliz = "bpns",
                                                   Orstedcod = "dpns",
                                                   FISHINTEL = "channel",
                                                   '2024_bovenschelde' = "zeeschelde",
                                                   CODEVCO_fish_detectors = "bpns",
                                                   FISP = "channel",
                                                   Grotenete = "rupel",
                                                   'Haringvliet2023-2026' = "voordelta",
                                                   PelFish = "bpns",
                                                   PureWind_fish_detectors = "bpns",
                                                   Walloneel = "albertkanaal")           


# Set region for five specific stations that are under two different projects (probably authority changed over the years)
deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("COU_01",
                                                                         "COU_09",
                                                                         "COU_11",
                                                                         "COU_12",
                                                                          "COU_14"),
                                       true = "channel",
                                       false = acoustic_project_code))


# Set region for a number of specific stations
deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("gn-1"),
                                         true = "rupel",
                                         false = acoustic_project_code))

deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("bpns-vandamme"),
                                         true = "boudewijnkanaal",
                                         false = acoustic_project_code))

deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("roche_burel"),
                                         true = "bay_of_biscay",
                                         false = acoustic_project_code))


deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("Haringvliet HD-B",
                                                                         "Haringvliet HV3"),
                                         true = "haringvliet",
                                         false = acoustic_project_code))

deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("Oude Maas BB A",
                                                                         "Oude Maas BB-C",
                                                                         "Oude Maas BB-E",
                                                                         "Nieuwe Maas NM 21",
                                                                         "Nieuwe Maas NM 23"),
                                         true = "maas",
                                         false = acoustic_project_code))

deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("Maasmond Maas 4",
                                                                         "NieuweWaterweg NW2",
                                                                         "NieuweWaterweg NW3",
                                                                         "Maasmond CA2-NW1",
                                                                         "Maasmond Maas 5",
                                                                         "CalandKanaal CA4",
                                                                         "BeerKanaal B1",
                                                                         "Hartelkanaal SB-E",
                                                                         "NieuweWaterweg NW8",
                                                                         "NieuweWaterweg NW25"),
                                         true = "nieuwe_waterweg",
                                         false = acoustic_project_code))


# Part of the FISHINTEL stations are in the Channel and part in BPNS
deployments <- deployments %>%
  mutate(acoustic_project_code = if_else(condition = station_name %in% c("bpns-Gardencity",
                                                                         "FISHINTEL_Bowsprite",
                                                                         "FISHINTEL_Gardencity_1",
                                                                         "FISHINTEL_Gardencity_2",
                                                                         "FISHINTEL_Gardencity_3",
                                                                         "FISHINTEL_Gardencity_4",
                                                                         "FISHINTEL_Gardencity_5",
                                                                         "FISHINTEL_Laura1",
                                                                         "FISHINTEL_Laura2",
                                                                         "FISHINTEL_Laura2_bis",
                                                                         "FISHINTEL_Laura3",
                                                                         "FISHINTEL_Laura4",
                                                                         "FISHINTEL_Laura5",
                                                                         "FISHINTEL_Laura6",
                                                                         "FISHINTEL_Mistwrak1",
                                                                         "FISHINTEL_Mistwrak2",
                                                                         "FISHINTEL_Mistwrak3",
                                                                         "FISHINTEL_Mistwrak4",
                                                                         "FISHINTEL_OHVS1",
                                                                         "FISHINTEL_OHVS2",
                                                                         "FISHINTEL_OHVS3",
                                                                         "FISHINTEL_OHVS4",
                                                                         "FISHINTEL_QuoVadis",
                                                                         "FISHINTEL_Radartoren2",
                                                                         "FISHINTEL_Radartoren3",
                                                                         "FISHINTEL_Radartoren4",
                                                                         "FISHINTEL_Radartoren5",
                                                                         "FISHINTEL_Radartoren6",
                                                                         "FISHINTEL_Radartoren7"),
                                         true = "bpns",
                                         false = acoustic_project_code))



# Remove duplicates (= stations with multiple acoustic project codes)
deployments <- distinct(deployments)

# Check if some station names occur more than once
number <- dplyr::count(deployments, station_name)

# Add acoustic_project_code to data
data <- left_join(data, deployments, by = "station_name")

# Change column name
data <- rename(data, location = acoustic_project_code)


# Add location to remaining stations based on station location file
locations <- read.csv("./data/external/station_locations.csv")

data <- left_join(data, locations, by = "station_name")
data <- data %>% 
  mutate(location.x = coalesce(location.x, location.y))
data$location.y <- NULL
data <- rename(data, location = location.x)

table(data$location)
