# Flag spawning migration
# LifeWatch 2023- 2024
# Damiano Oldoni: damiano.oldoni@inbo.be
# Pieterjan Verhelst: pieterjan.verhelst@inbo.be

# Source functions
#source("./src/add_location.R")
source("./src/flag_tagging_effect.R")
source("./src/detect_spawning_migration_functions_first_derivative_smoother_method.R")

# Filter data with shads detected the year after tagging and hence showing spawning migration
ids <- c(
  # year 2019
  "1308534","1308536", 
  # year 2020
   "1264423","1264425","1264426","1308527","1308530","1308531","1308533","1308537",
  # year 2021
  "1367854","1367858","1367860","1367861","1367862","1367863","1367868","1367869","1367874","1367877","1367881",
  # year 2022
  "1367836","1367844","1367846","22084650","22084651","22084652","22084653","22084654","22084658","22084665","22084666","22084670","22084672","22084673","22084674","22084679","22084687",
  # 2022 died during spawning (no downstream spawning migration)
  #"1367838","22084683",
  # 2023
  "02BT","02BY","02CC","02CS","02CV","02CZ","02D3","23087255","23087261","23087270","23087271","23087277",
  # 2023 died during spawning (no downstream spawning migration)
  #"02BS","02C7","02CD","23087274","23087278"
# 2024
"05FB","05FI","05FJ","05FQ","05FY","05G2","05GL","05GX","05GZ","05HH","05I7"#,
# 2024 died during spawning (no downstream spawning migration)
#"05F9","05GC","05H8"
)  

returners <- data_with_tagging_effects[data_with_tagging_effects$tag_serial_number %in% ids, ]
returners$tag_serial_number <- factor(returners$tag_serial_number)
unique(returners$tag_serial_number)


# define spawn_locations, i.e. the location(s) where spawning takes place.
spawn_locations <- c("zeeschelde", "rupel")

# do we use the flagging `tagging_effect` column?
tagging_flagged <- TRUE



# 1. Apply to one fish ####

tag_id_example <- "1264423"
# tag_id_example <- "1308536"
# tag_id_example <- "1264423"
# tag_id_example <- "02CV"
# tag_id_example <- "23087274" # loess should fail (no spawning, no downstream)

assertthat::assert_that(
  tag_id_example %in% unique(returners$tag_serial_number),
  msg = "{tag_id_example} not found in returners$tag_serial_number"
)
# Example with one fish
shad <- dplyr::filter(returners,
                      tag_serial_number == tag_id_example
)
# View(shad)

# Test "loess"
# One of "loess", "gam"
method_smooth <- "loess"
shad_flagged <- get_spawning_migrations(shad,
                              spawning_period = 100,
                              method_smooth = method_smooth,
                              span = 0.10,
                              upstream_speed_threshold = -0.09,
                              downstream_speed_threshold = 0.09,
                              spawning_distance_limit = 2000)
# Start spawning period: minimum arrival date when detection_window is TRUE
start_spawning_period <- min(shad_flagged$arrival[shad_flagged$detection_window == TRUE])
# End spawning period: maximum arrival date when detection_window is TRUE
end_spawning_period <- max(shad_flagged$arrival[shad_flagged$detection_window == TRUE])

# Check spawning migration via plot
# View(shad_flagged)

p <- plot_migration_detection(shad_flagged,
                              start_spawning_period,
                              end_spawning_period)
plotly::plotly_build(p)





# 2. Apply to all fishes ####

# Select and group IDs according to model parameters

# Group 1 ####
# spawning period = 100
# span = 0.1
# upstream speed = 0.09
# downstream speed = 0.09
ids <- c("1308534","1308536","1264423","1264425","1308530","1308531","1308533","1308537", "1367854","1367858","1367861","1367868","1367869","1367874","1367877","1367881","1367836","1367846","22084651","22084653","22084654","22084658","22084666","22084670","22084672","22084673","22084679","02BT","02BY","02CC","02CS","02CV","02CZ","02D3","23087255","23087261","23087270","23087271","23087277","05FI","05FY","05G2","05GL","05GZ","05HH","05I7")  


# Group 2 ####
# spawning period = 50
# span = 0.1
# upstream speed = 0.09
# downstream speed = 0.09
ids <- c("1264426","1308527","1367860","1367862","1367844","22084650","22084674","05FB")  


# Group 3 ####
# spawning period = 100
# span = 0.2
# upstream speed = 0.09
# downstream speed = 0.09
ids <- c("22084652","1367863","22084665","22084687","05FJ","05FQ")  


# Group 4 ####
# spawning period = 50
# span = 0.2
# upstream speed = 0.09
# downstream speed = 0.09
ids <- c("05GX")  


returners <- data_with_tagging_effects[data_with_tagging_effects$tag_serial_number %in% ids, ]
returners$tag_serial_number <- factor(returners$tag_serial_number)
unique(returners$tag_serial_number)


# define spawn_locations, i.e. the location(s) where spawning takes place.
spawn_locations <- c("zeeschelde", "rupel")

# do we use the flagging `tagging_effect` column?
tagging_flagged <- TRUE


# Apply methodology
method_smooth <- "loess"
data_with_spawning_behavior <- 
  returners %>%
  # dplyr::filter(tag_serial_number == tag_id_example) %>%
  dplyr::mutate(tag_serial_number_value = as.character(tag_serial_number)) %>%
  dplyr::group_by(tag_serial_number_value) %>%
  tidyr::nest() %>%
  dplyr::mutate(tagging = purrr::map(
    data,
    function(x) {
      message(paste("Processing acoustic tag:", unique(x$tag_serial_number)))
      get_spawning_migrations(x,
                              spawning_period = 100,
                              method_smooth = method_smooth,
                              span = 0.10,
                              upstream_speed_threshold = -0.09,
                              downstream_speed_threshold = 0.09,
                              spawning_distance_limit = 2000)
      })) %>%
  dplyr::select(-data) %>%
  tidyr::unnest(tagging) %>%
  dplyr::ungroup()


# Bind three datasets together
data_with_spawning_behavior <- rbind(data_with_spawning_behavior, data_with_spawning_behavior2)
data_with_spawning_behavior <- rbind(data_with_spawning_behavior, data_with_spawning_behavior3)
data_with_spawning_behavior <- rbind(data_with_spawning_behavior, data_with_spawning_behavior4)


# check some fishes
output_shad <- data_with_spawning_behavior %>% 
  filter(tag_serial_number == "1308534")
sel1 <- output_shad %>% 
  select(tag_serial_number, station_name, arrival, location, migration)


# Create plot to check the flagging of spawning migration
plot <- ggplot() + geom_line(aes(arrival, -1*distance_to_source_m), data = output_shad, colour = "black", size = 1) + 
  geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = output_shad, shape = 1, size = 5) + 
  #ggtitle(data2$tag_serial_number) +  
  labs(title = output_shad$tag_serial_number, subtitle = output_shad$catch_year) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
  ylab("Distance (km)") + 
  xlab("Date") + 
  scale_y_continuous(limit = c(-200000, 40000),
                     breaks = c(-200000,-190000,-180000,-170000,-160000,-150000,-140000,-130000,-120000,-110000,-100000,-90000,-80000,-70000,-60000,-50000,-40000,-30000,-20000,-10000,0,10000,20000,30000,40000), 
                     labels = c(-200,-190,-180,-170,-160,-150,-140,-130,-120,-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  scale_x_datetime(date_breaks  ="1 month") + 
  geom_hline(yintercept = -1*output_shad$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed") +
  annotate("text",x = output_shad$arrival[1] - (1000*60*60), y = -1*output_shad$distance_to_source_m, label = output_shad$station_name, hjust=0, colour="red", size = 3)

plot



# check for spawning section specifically
output_shad <- data_with_spawning_behavior %>% 
  filter(tag_serial_number == "1308534",
         migration == "upstream" | migration == "spawning" | migration == "downstream")
sel1 <- output_shad %>% 
  select(tag_serial_number, station_name, arrival, location, migration)
#View(sel1)


# Create plot to check the flagging of spawning migration
plot <- ggplot() + geom_line(aes(arrival, -1*distance_to_source_m), data = output_shad, colour = "black", size = 1) + 
  geom_point(aes(arrival, -1*distance_to_source_m, colour = migration), data = output_shad, shape = 1, size = 5) + 
  #ggtitle(data2$tag_serial_number) +  
  labs(title = output_shad$tag_serial_number) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
  ylab("Distance (km)") + 
  xlab("Date") + 
  scale_y_continuous(limit = c(-140000, 40000),
                     breaks = c(-120000,-110000,-100000,-90000,-80000,-70000,-60000,-50000,-40000,-30000,-20000,-10000,0,10000,20000,30000,40000), 
                     labels = c(-120,-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.title.y = element_text(size = 22)) +
  scale_x_datetime(date_breaks  ="1 day") + 
  geom_hline(yintercept = -1*output_shad$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed") +
  annotate("text",x = output_shad$arrival[1] - (300*60*60), y = -1*output_shad$distance_to_source_m, label = output_shad$station_name, hjust=0, colour="red", size = 3)

plot



