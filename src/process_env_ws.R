# Process environmental data of the Westerschelde obtained via Rijkswaterstaat Waterinfo platform
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load libraries
library(tidyverse)
library(lubridate)


# Read coordinates of stations ####
coors <- read.csv('./data/external/locations_environmental_stations_westerschelde.csv')
coors$location <- factor(coors$location)


# Read dissolved oxygen ####
#oxygen1 <- read.csv('./data/external/environmental_variables/westerschelde/dissolved_oxygen/20260223_103.csv', sep = ";") 
#oxygen2 <- read.csv('./data/external/environmental_variables/westerschelde/dissolved_oxygen/20260416-4253.csv', sep = ";") 
#oxygen3 <- read.csv('./data/external/environmental_variables/westerschelde/dissolved_oxygen/20260416-4256.csv', sep = ";") 
#oxygen4 <- read.csv('./data/external/environmental_variables/westerschelde/dissolved_oxygen/20260416-4257.csv', sep = ";") 

files <- list.files(path = "./data/external/environmental_variables/westerschelde/dissolved_oxygen/", pattern = "*.csv", full.names = TRUE)

oxygen <- files %>% 
  map_df(~read_csv2(., col_types = cols(.default = "c")))


oxygen <- oxygen %>%
  dplyr::select(MEETPUNT_IDENTIFICATIE, WAARNEMINGDATUM, WAARNEMINGTIJD, PARAMETER_OMSCHRIJVING, 'PARAMETER_ CODE', NUMERIEKEWAARDE, EENHEID_CODE) %>%
  rename(location = MEETPUNT_IDENTIFICATIE,
         date = WAARNEMINGDATUM,
         time = WAARNEMINGTIJD,
         parameter = PARAMETER_OMSCHRIJVING,
         parameter_code = 'PARAMETER_ CODE',
         value = NUMERIEKEWAARDE,
         unit = EENHEID_CODE)#%>%
#  mutate(value = readr::parse_number(as.character(value), locale = readr::locale(decimal_mark = ",")))


oxygen$location <- factor(oxygen$location)

oxygen <- oxygen %>%
  mutate(location = fct_recode(location,
                               "Vlissingen boei SSVH" = "Vlissingen, boei SSVH",
                               "Terneuzen boei 20" = "Terneuzen, boei 20",
                               "Hansweert geul" = "Hansweert, geul"
  ))

oxygen$value <- as.numeric(oxygen$value)

# Link coordinates
oxygen <- left_join(oxygen, coors, by = "location") %>%
  filter(value < 1000)




# Read Chloride ####
files <- list.files(path = "./data/external/environmental_variables/westerschelde/conductivity/", pattern = "*.csv", full.names = TRUE)

chloride <- files %>% 
  map_df(~read_csv2(., col_types = cols(.default = "c")))

chloride <- chloride %>%
  dplyr::select(MEETPUNT_IDENTIFICATIE, WAARNEMINGDATUM, WAARNEMINGTIJD, GROOTHEID_OMSCHRIJVING, 'GROOTHEID_ CODE', NUMERIEKEWAARDE, EENHEID_CODE) %>%
  rename(location = MEETPUNT_IDENTIFICATIE,
         date = WAARNEMINGDATUM,
         time = WAARNEMINGTIJD,
         parameter = GROOTHEID_OMSCHRIJVING,
         parameter_code = 'GROOTHEID_ CODE',
         value = NUMERIEKEWAARDE,
         unit = EENHEID_CODE) %>%
  mutate(value = readr::parse_number(as.character(value), locale = readr::locale(decimal_mark = ".", grouping_mark = ",")))

chloride$location <- factor(chloride$location)

# Change column level names
chloride <- chloride %>%
  mutate(location = fct_recode(location,
                           "Baalhoek" = "Kloosterzande, Baalhoek",
                           "Terneuzen boei 20" = "Terneuzen, westsluis, buiten"
  ))

# Link coordinates
chloride <- chloride %>% left_join(coors, by = "location") %>%
  filter(value < 100000) %>%
  mutate(value = value*10,
  unit = "\u03bcS/cm")



# Read water temperature ####
temperature <- read.csv('./data/external/environmental_variables/westerschelde/20260224_045.csv', sep = ";") 
temperature2025 <- read.csv('./data/external/environmental_variables/westerschelde/20260416-4248.csv', sep = ";") 

temperature <- temperature %>%
  dplyr::select(MEETPUNT_IDENTIFICATIE, WAARNEMINGDATUM, WAARNEMINGTIJD..MET.CET., GROOTHEID_OMSCHRIJVING, NUMERIEKEWAARDE, EENHEID_CODE) %>%
  rename(location = MEETPUNT_IDENTIFICATIE,
         date = WAARNEMINGDATUM,
         time = WAARNEMINGTIJD..MET.CET.,
         parameter = GROOTHEID_OMSCHRIJVING,
         #parameter_code = PARAMETER_.CODE,
         value = NUMERIEKEWAARDE,
         unit = EENHEID_CODE) %>%
  mutate(value = readr::parse_number(as.character(value), locale = readr::locale(decimal_mark = ",")))


temperature2025 <- temperature2025 %>%
  dplyr::select(MEETPUNT_IDENTIFICATIE, WAARNEMINGDATUM, WAARNEMINGTIJD, GROOTHEID_OMSCHRIJVING, NUMERIEKEWAARDE, EENHEID_CODE) %>%
  rename(location = MEETPUNT_IDENTIFICATIE,
         date = WAARNEMINGDATUM,
         time = WAARNEMINGTIJD,
         parameter = GROOTHEID_OMSCHRIJVING,
         #parameter_code = PARAMETER_.CODE,
         value = NUMERIEKEWAARDE,
         unit = EENHEID_CODE) %>%
  mutate(value = readr::parse_number(as.character(value), locale = readr::locale(decimal_mark = ",")))

temperature <- rbind(temperature, temperature2025)

temperature$location <- factor(temperature$location)

temperature <- temperature %>%
  mutate(location = fct_recode(location,
                                "Vlissingen boei SSVH" = "Vlissingen, boei SSVH",
                                "Terneuzen boei 20" = "Terneuzen, boei 20",
                               "Baalhoek" = "Kloosterzande, Baalhoek",
                               "Hansweert geul" = "Hansweert, geul"
  ))


# Link coordinates
temperature <- temperature %>% left_join(coors, by = "location") %>%
  filter(value < 50)

# Calculate average temperature
# month_year_temp <- temperature %>%
#   group_by(year, month) %>%
#   summarize(
#     mean_temp_month = mean(value, na.rm = TRUE),
#     .groups = "drop" # Zorgt dat de groepering daarna stopt
#   )

# mean_spring <- filter(month_year_temp, month == "3")

# write.csv(mean_spring, "./data/external/mean_temp_march.csv")


# Read water level ####
level2019 <- read.csv('./data/external/environmental_variables/westerschelde/water_level/2019/20260301-414.csv', sep = ";") 
level2020 <- read.csv('./data/external/environmental_variables/westerschelde/water_level/2020/20260301-415.csv', sep = ";") 
level2021 <- read.csv('./data/external/environmental_variables/westerschelde/water_level/2021/20260301-416.csv', sep = ";") 
level2022 <- read.csv('./data/external/environmental_variables/westerschelde/water_level/2022/20260301-417.csv', sep = ";") 
level2023 <- read.csv('./data/external/environmental_variables/westerschelde/water_level/2023/20260301-418.csv', sep = ";") 
level2024 <- read.csv('./data/external/environmental_variables/westerschelde/water_level/2024/20260301-421.csv', sep = ";") 
level2025 <- read.csv('./data/external/environmental_variables/westerschelde/water_level/2025/20260301-422.csv', sep = ";") 


# Bind datasets together
level <- rbind(level2019, level2020)
level <- rbind(level, level2021)
level <- rbind(level, level2022)
level <- rbind(level, level2023)
level <- rbind(level, level2024)
level <- rbind(level, level2025)

# Remove yearly datasets to clean R environment
rm(level2019,
   level2020,
   level2021,
   level2022,
   level2023,
   level2024,
   level2025)

# Format dataset
level <- level %>%
  dplyr::select(MEETPUNT_IDENTIFICATIE, WAARNEMINGDATUM, WAARNEMINGTIJD, LAT, LON, GROOTHEID_OMSCHRIJVING, NUMERIEKEWAARDE, EENHEID_CODE) %>%
  rename(location = MEETPUNT_IDENTIFICATIE,
         date = WAARNEMINGDATUM,
         time = WAARNEMINGTIJD,
         latitude = LAT,
         longitude = LON,
         parameter = GROOTHEID_OMSCHRIJVING,
         #parameter_code = PARAMETER_.CODE,
         value = NUMERIEKEWAARDE,
         unit = EENHEID_CODE)

level$location <- factor(level$location)

coors_level <- unique(level[c("location", "latitude", "longitude")])
coors <- rbind(coors, coors_level)
# Link coordinates
# In the water level sensors, the WGS84 coordinates come with the sensor files, so no need for linkage with external dataset.

# Calculate tide ####
tide_candidates <- level %>%
  mutate(
    Timestamp = suppressWarnings(dmy_hms(paste(date, time))),
    value = na_if(value, 999999999)# 999999999 are the NA values
  ) %>%
  arrange(Timestamp) %>%
  group_by(location) %>%
  mutate(
    # Extract the maximum and minimum water levels over a rolling window (width = 25, +- 6 hours of data at 10-minute intervals).
    # Filters out the smaller fluctuations
    roll_max = zoo::rollapply(value, width = 36, FUN = function(x) if(all(is.na(x))) NA_real_ else max(x, na.rm = TRUE), align = "center", fill = NA),
    roll_min = zoo::rollapply(value, width = 36, FUN = function(x) if(all(is.na(x))) NA_real_ else min(x, na.rm = TRUE), align = "center", fill = NA),
    is_hw = !is.na(roll_max) & (value == roll_max),
    is_lw = !is.na(roll_min) & (value == roll_min),
    tide = case_when(
      is_hw ~ "HW",
      is_lw ~ "LW",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(tide)) %>%
  ungroup()%>% dplyr::select(-is_hw, -is_lw, -value)%>%
  rename(value = tide)

tide_test <- tide_candidates %>%
  group_by(location) %>%
  mutate(change_id = cumsum(coalesce(tide != lag(tide), TRUE))) %>%#filter out cosecutive LWs or HWs
  group_by(location, change_id, tide) %>%
  slice(if (first(tide) == "HW") which.max(value) else which.min(value)) %>%
  ungroup() %>%
  dplyr::select(-is_hw, -is_lw, -change_id, -value)%>%
  rename(value = tide)