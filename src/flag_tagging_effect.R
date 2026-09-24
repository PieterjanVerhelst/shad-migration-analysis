# Remove tagging effect: the moment the shads are in the BPNS upon tagging and do not return to the Zeeschelde anymore during the first 40 days of tracking
# By Pieterjan Verhelst and Damiano Oldoni
# pieterjan.verhelst@inbo.be, damiano.oldoni@inbo.be


# Source 
source("./src/add_location.R")
source("./src/detect_tagging_effect.R")

# define the tagging location
tag_location <- "zeeschelde"  # default value

# define end tagging effects location 
end_location <- "bpns"  # default value

# Example with one fish
shad <- dplyr::filter(data, tag_serial_number == "1367836")
# 10 days tagging effects window
shad_output_10_days <- detect_tagging_effect(shad, n_days = 10)
# 40 days tagging effects window
shad_output_40_days <- detect_tagging_effect(shad, n_days = 40)

sel1 <- select(shad_output_10_days, tag_serial_number, station_name, arrival, location, tagging_effect)
sel2 <- select(shad_output_40_days, tag_serial_number, station_name, arrival, location, tagging_effect)
#View(sel2)



# Apply to all fishes with 40 days of tagging
data_with_tagging_effects <- 
  data %>%
  group_by(tag_serial_number) %>%
  nest() %>%
  mutate(tagging = map(data, detect_tagging_effect, 40)) %>%
  select(-data) %>%
  unnest(tagging) %>%
  ungroup()

# check some fishes
input_shad <- dplyr::filter(data, tag_serial_number == "1367836")
output_shad <- dplyr::filter(data_with_tagging_effects,
                             tag_serial_number == "1367836"
)
sel3 <- select(output_shad, tag_serial_number, station_name, arrival, location, tagging_effect)
#View(sel3)




