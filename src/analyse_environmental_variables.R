# Determine the environmental conditions during upstream migration, spawning and downstream migration, and test for differences between years.
# By Pieterjan Verhelst & Hanna Jaspaert
# pieterjan.verhelst@inbo.be & hanna.jaspaert@ugent.be

# This part relies on the scripts in /src/env_variables/ which were developed by Hanna Jaspaert


# Load packages ####
library(tidyverse)
library(suncalc)
library(nlme)
library(lme4)
library(lmerTest)
library(MASS)
library(car)
library(performance)
library(MuMIn)
library(sandwich)
library(lmtest)
library(sjPlot)
library(interactions)
library(effects)


# 1. Load data with environmental variables ####
data  <- read_csv('./data/interim/data_with_env_variables.csv') 
data <- dplyr::select(data, -...1,-.id,-...3,-...4,-animal_project_code,-receiver_id,-capture_method,-release_location,-tagging_type,-tagging_effect,-detection_window,-smoothing_window,-smooth_upper,-smooth_lower,-smoothed_distance,-first_derivative,-is_down_tov_ref) 

# Add year
data$year <- factor(year(data$arrival))

# Add date
data$date <- as.Date(data$arrival)

# Add photoperiod
# Step 1: adjust column names so suncalc recognises them
data <- data %>%
  mutate(
   # date = as.Date(datum),  # suncalc requires name 'date'
    lat  = deploy_latitude,        # change to lat
    lon  = deploy_longitude        # change to lon
  )

# Step 2: calculate sunrise and sunset for every row
sun_data <- getSunlightTimes(
  data = data,
  keep = c("sunrise", "sunset"),
  tz = "UTC" # Pas aan naar jouw tijdzone, bijv. "Europe/Brussels"
)

# Step 3: calculate photoperiod and add to the dataset
data <- data %>%
  mutate(
    sunrise = sun_data$sunrise,
    sunset  = sun_data$sunset,
    photoperiod = as.numeric(sunset - sunrise) # day length in hours
  )

data <- dplyr::select(data, -lat, -lon, -sunrise, -sunset)

# Set unrealistic high temperature values to NA
data <- data %>%
  mutate(temp_at_arrival = ifelse(temp_at_arrival > 22, NA, temp_at_arrival))


# Link average March temperatures to the data
march_temp  <- read_csv('./data/external/mean_temp_march.csv') 
march_temp$year <- factor(march_temp$year)
march_temp <- dplyr::select(march_temp, -...1, -month)
data <- left_join(data, march_temp, by = "year")


# 2. Filter different behaviours ####
spawning <- filter(data, behaviour == "spawning")
upstream <- filter(data, behaviour == "upstream")
downstream <- filter(data, behaviour == "downstream")

# 3. Calculate summary environmental conditions for upstream and downstream migration ####
summary(upstream[c("temp_at_arrival", "do_at_arrival", "turb_at_arrival", "cond_at_arrival")])

summary <- upstream %>%
  summarise(across(
    .cols = c(temp_at_arrival, do_at_arrival, turb_at_arrival, cond_at_arrival),
    .fns = list(
      mean = ~mean(.x, na.rm = TRUE),
      SD = ~sd(.x, na.rm = TRUE),
      minimum = ~min(.x, na.rm = TRUE),
      maximum = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

# 4. Calculate summary of the environmental conditions for the 50% core area of spawning ####

# Step 1: Calculate the borders of the fish for the 50% core use area
core_bounds <- spawning %>%
  group_by(tag_serial_number) %>%
  summarise(
    pos_25 = quantile(distance_to_source_m, probs = 0.25, na.rm = TRUE),
    pos_75 = quantile(distance_to_source_m, probs = 0.75, na.rm = TRUE)
  )

# Step 2: Link the borders back to the data and filter the detections
habitat_50_data <- spawning %>%
  inner_join(core_bounds, by = "tag_serial_number") %>%
  filter(distance_to_source_m >= pos_25 & distance_to_source_m <= pos_75)

# Step 3: Calculate the summaries of the environmental data
summary_environmental_vars <- habitat_50_data %>%
  #group_by(tag_serial_number) %>% # Remove this if you want the summary of all shads
  summarise(across(
    .cols = c(temp_at_arrival, do_at_arrival, turb_at_arrival, cond_at_arrival), 
    .fns = list(
      mean  = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      min  = ~min(.x, na.rm = TRUE),
      max  = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))



# 5. Environmental conditions during start upstream migration ####
# Filter records of start upstream migration
start_upstream <- upstream %>%
  group_by(tag_serial_number) %>%
  filter(arrival == min(arrival, na.rm = TRUE)) %>%
  ungroup()

# Create exploratory boxplot
ggplot(start_upstream, aes(x=year, y=temp_at_arrival)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Variable") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Set starting date as day of the year (= day number)
start_upstream$daynumber <- yday(start_upstream$arrival)

# Explorative boxplot
ggplot(start_upstream, aes(x = year, y = daynumber)) +
  geom_boxplot() +
  ylab("Day of the year") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Analyse difference in start of migration between years in relation to water temperature
year_summary <- start_upstream %>%
  group_by(year) %>%
  summarise(
    mean_daynumber = mean(daynumber)# of de waarde die bij dat jaar hoort
  )

year_summary <- left_join(year_summary, march_temp, by = "year")

ggplot(year_summary, aes(x = mean_temp_month, y = mean_daynumber)) +
  geom_point(size = 4, color = "black") +
  geom_text(aes(label = year), vjust = -1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  labs(#title = "Annual deviation vs average spring temperature",
    x = "Average March temperature (°C)",
    y = "Average day to start upstream migration") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")

# Run linear model
model <- lm(mean_daynumber ~ mean_temp_month, data = year_summary)
summary(model)

par(mfrow = c(2, 2))
plot(model)



# Apply GLMM

# Check number of shads per year
start_upstream %>%
  group_by(year) %>%
  summarise(n = n_distinct(tag_serial_number))

# Check locations
unique(start_upstream$location)

# Remove start location as zeeschelde because these are shads that were missed in the Westerschelde, but had started their upstream migration some time ago
start_upstream <- filter(start_upstream, location != "zeeschelde")
start_upstream <- filter(start_upstream, location != "ws3")
# => This removes 7 shads

# Scale variables
start_upstream <- start_upstream %>%
  mutate(
    temp_at_arrival_z      = as.numeric(scale(temp_at_arrival)),
    do_z  = as.numeric(scale(do_at_arrival)),
    cond_z      = as.numeric(scale(cond_at_arrival))
  ) 


# Remove data from year 2020 since it only has two data points, making it dubious to run a model
#start_upstream <- filter(start_upstream, year != "2020")

# Remove two shads because they caused heterscedasticity, failing the statistical models to converge
#start_upstream <- filter(start_upstream, tag_serial_number != "1367854")
#start_upstream <- filter(start_upstream, tag_serial_number != "05FB")

# Correlation between photoperiod and daynumber
cor(start_upstream$photoperiod, start_upstream$daynumber)



# Step 1: Conduct GLMM
model <- lmer(daynumber ~ temp_at_arrival_z + do_z + cond_z + sex + #turb_z +
                temp_at_arrival_z : do_z +
                temp_at_arrival_z : cond_z +
                do_z : cond_z #+
              #temp_z : turb_z +
              #do_z : turb_z +
              #cond_z : turb_z
              + (1 | year), 
              data = start_upstream)

summary(model)
anova(model) 

# Step 2: automatic stepwise selection to identify best variable
#step_result <- step(model)
#step_result

# Step 3: extract best model
#final_model <- get_model(step_result)
#summary(final_model)

# Step 2: manual stepwise selection by backwards elimination based on AIC
model1 <- lmer(daynumber ~ temp_at_arrival_z + do_z + cond_z + sex + 
                 temp_at_arrival_z : do_z +
                 temp_at_arrival_z : cond_z +
                 do_z : cond_z + 
                 (1 | year), 
               data = start_upstream,
               REML = FALSE)

model2 <- lmer(daynumber ~ temp_at_arrival_z + do_z + cond_z + #sex + 
                 temp_at_arrival_z : do_z +
                 temp_at_arrival_z : cond_z +
                 do_z : cond_z + 
                 (1 | year), 
               data = start_upstream,
               REML = FALSE)


AIC(model1, model2)

summary(model2)

# Step 3: Identify best model
final_model <- lmer(daynumber ~ temp_at_arrival_z + do_z + cond_z + #sex + 
                      temp_at_arrival_z : do_z +
                      temp_at_arrival_z : cond_z +
                      do_z : cond_z +
               (1 | year),
               data = start_upstream#,
               #REML = FALSE   # Make sure to use REML again when running the final model
               )

summary(final_model)
AIC(final_model)


# Create table with model output
table_data <- tidy(final_model)

# Step 4: plot effect of model
plot(allEffects(final_model))

plot_model(final_model, type = "int", terms = c("temp_z", "do_z"))
plot_model(final_model, type = "int", terms = c("do_z", "cond_z"))

# Step 5: check model performance and validation
# Marginal R²: variance explained by fixed effects
# Conditional R²: variance explained by the total model
performance::r2(final_model)

plot(final_model) 

qqnorm(residuals(final_model))
qqline(residuals(final_model))

# Step 6: analyse if shads arrive earlier in warm years
# Extract random effects
random_effects <- ranef(final_model)$year

# Put them in a table
year_deviations <- data.frame(
  year = rownames(random_effects),
  deviation_days = random_effects[,1]
)
print(year_deviations)

spring_temp <- start_upstream %>%
  group_by(year) %>%
  summarize(
    mean_temp = mean(temp_at_arrival, na.rm = TRUE)
  )

year_deviations <- left_join(year_deviations, spring_temp, by = "year")

ggplot(year_deviations, aes(x = mean_temp, y = deviation_days)) +
  geom_point(size = 4, color = "black") +
  geom_text(aes(label = year), vjust = -1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  labs(#title = "Annual deviation vs average spring temperature",
    x = "Average spring temperature (°C)",
    y = "Deviation in arrival (days)") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")







# 6. Environmental conditions during arrival at spawning sites ####
# Filter records of first spawning (= arrival spawning)
arrival_spawning <- spawning %>%
  group_by(tag_serial_number) %>%
  filter(arrival == min(arrival, na.rm = TRUE)) %>%
  ungroup()

# Create exploratory boxplot
ggplot(arrival_spawning, aes(x=year, y=temp_at_arrival)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Variable") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Set starting date as day of the year (= day number)
arrival_spawning$daynumber <- yday(arrival_spawning$arrival)

# Explorative boxplot
ggplot(arrival_spawning, aes(x = year, y = daynumber)) +
  geom_boxplot() +
  ylab("Day of the year") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Analyse difference in arrival at spawning between years in relation to water temperature
year_summary <- arrival_spawning %>%
  group_by(year) %>%
  summarise(
    mean_daynumber = mean(daynumber)# of de waarde die bij dat jaar hoort
  )

year_summary <- left_join(year_summary, march_temp, by = "year")

ggplot(year_summary, aes(x = mean_temp_month, y = mean_daynumber)) +
  geom_point(size = 4, color = "black") +
  geom_text(aes(label = year), vjust = -1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  labs(#title = "Annual deviation vs average spring temperature",
    x = "Average March temperature (°C)",
    y = "Average day to arrive at spawning grounds") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")

# Run linear model
model <- lm(mean_daynumber ~ mean_temp_month, data = year_summary)
summary(model)

par(mfrow = c(2, 2))
plot(model)




# Apply GLMM

# Check number of shads per year
arrival_spawning %>%
  group_by(year) %>%
  summarise(n = n_distinct(tag_serial_number))

# Check locations
unique(arrival_spawning$location)

# Scale variables
arrival_spawning <- arrival_spawning %>%
  mutate(
    temp_at_arrival_z      = as.numeric(scale(temp_at_arrival)),
    do_z  = as.numeric(scale(do_at_arrival)),
    cond_z      = as.numeric(scale(cond_at_arrival)),
    turb_z      = as.numeric(scale(turb_at_arrival))
  ) 


# Step 1: Conduct GLMM
model <- lmer(daynumber ~ temp_at_arrival_z + do_z + cond_z + sex + #turb_z +
                temp_at_arrival_z : do_z +
                temp_at_arrival_z : cond_z +
                do_z : cond_z #+
              #temp_z : turb_z +
              #do_z : turb_z +
              #cond_z : turb_z
              + (1 | year), 
              data = arrival_spawning)

summary(model)
anova(model) 


# Step 2: automatic stepwise selection to identify best variable
#step_result <- step(model)
#step_result

# Step 3: extract best model
#final_model <- get_model(step_result)
#summary(final_model)

# Step 2: manual stepwise selection by backwards elimination based on AIC
model1 <- lmer(daynumber ~ temp_at_arrival_z + do_z + cond_z + sex +
                 temp_at_arrival_z : do_z +
                 temp_at_arrival_z : cond_z +
                 do_z : cond_z +
                 (1 | year), 
               data = arrival_spawning,
               REML = FALSE)

model2 <- lmer(daynumber ~ temp_at_arrival_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 #temp_at_arrival_z : do_z +
                 temp_at_arrival_z : cond_z +
                 do_z : cond_z +
                 (1 | year), 
               data = arrival_spawning,
               REML = FALSE)

model3 <- lmer(daynumber ~ temp_at_arrival_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 #temp_at_arrival_z : do_z +
                 temp_at_arrival_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = arrival_spawning,
               REML = FALSE)

model4 <- lmer(daynumber ~ temp_at_arrival_z + 
                 do_z + 
                 cond_z + 
                 #sex +
                 #temp_at_arrival_z : do_z +
                 temp_at_arrival_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = arrival_spawning,
               REML = FALSE)


model5 <- lmer(daynumber ~ temp_at_arrival_z + 
                 #do_z + 
                 cond_z + 
                 #sex +
                 #temp_at_arrival_z : do_z +
                 temp_at_arrival_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = arrival_spawning,
               REML = FALSE)


AIC(model4, model5)

summary(model5)

# Step 3: Identify best model
final_model <- lmer(daynumber ~ temp_at_arrival_z + 
                      cond_z + 
                      temp_at_arrival_z : cond_z +
                      (1 | year), 
                    data = arrival_spawning #,
                    #REML = FALSE   # Make sure to use REML again when running the final model
)

summary(final_model)


# Create table with model output
table_data <- tidy(final_model)

# Step 4: check model performance and validation
# Marginal R²: variance explained by fixed effects
# Conditional R²: variance explained by the total model
performance::r2(final_model)

plot(final_model) 

qqnorm(residuals(final_model))
qqline(residuals(final_model))

# Step 5: analyse if shads arrive earlier in warm years
# Extract random effects
random_effects <- ranef(final_model)$year

# Put them in a table
year_deviations <- data.frame(
  year = rownames(random_effects),
  deviation_days = random_effects[,1]
)
print(year_deviations)


spring_temp <- arrival_spawning %>%
  group_by(year) %>%
  summarize(
    mean_temp = mean(temp_at_arrival, na.rm = TRUE)
  )

year_deviations <- left_join(year_deviations, spring_temp, by = "year")

ggplot(year_deviations, aes(x = mean_temp, y = deviation_days)) +
  geom_point(size = 4, color = "black") +
  geom_text(aes(label = year), vjust = -1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  labs(#title = "Annual deviation vs average spring temperature",
       x = "Average spring temperature (°C)",
       y = "Deviation in arrival (days)") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")




# 7. Environmental conditions during departure at spawning sites ####
# Filter records of last spawning (= departure spawning)
departure_spawning <- spawning %>%
  group_by(tag_serial_number) %>%
  filter(departure == max(departure, na.rm = TRUE)) %>%
  ungroup()

# Create exploratory boxplot
ggplot(departure_spawning, aes(x=year, y=temp_at_arrival)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Variable") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Set starting date as day of the year (= day number)
departure_spawning$daynumber <- yday(departure_spawning$arrival)

# Explorative boxplot
ggplot(departure_spawning, aes(x = year, y = daynumber)) +
  geom_boxplot() +
  ylab("Day of the year") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Analyse difference in arrival at spawning between years in relation to water temperature
year_summary <- departure_spawning %>%
  group_by(year) %>%
  summarise(
    mean_daynumber = mean(daynumber)# of de waarde die bij dat jaar hoort
  )

year_summary <- left_join(year_summary, march_temp, by = "year")

ggplot(year_summary, aes(x = mean_temp_month, y = mean_daynumber)) +
  geom_point(size = 4, color = "black") +
  geom_text(aes(label = year), vjust = -1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  labs(#title = "Annual deviation vs average spring temperature",
    x = "Average March temperature (°C)",
    y = "Average day to depart from spawning grounds") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")

# Run linear model
model <- lm(mean_daynumber ~ mean_temp_month, data = year_summary)
summary(model)

par(mfrow = c(2, 2))
plot(model)



# Apply GLMM

# Check number of shads per year
departure_spawning %>%
  group_by(year) %>%
  summarise(n = n_distinct(tag_serial_number))

# Check locations
unique(arrival_spawning$location)

# Scale variables
departure_spawning <- departure_spawning %>%
  mutate(
    temp_z      = as.numeric(scale(temp_at_arrival)),
    do_z  = as.numeric(scale(do_at_arrival)),
    cond_z      = as.numeric(scale(cond_at_arrival)),
    turb_z      = as.numeric(scale(turb_at_arrival))
  ) 

# Step 1: Conduct GLMM
model <- lmer(daynumber ~ temp_z + do_z + cond_z + sex + #turb_z +
                temp_z : do_z +
                temp_z : cond_z +
                do_z : cond_z #+
              #temp_z : turb_z +
              #do_z : turb_z +
              #cond_z : turb_z
              + (1 | year), 
              data = departure_spawning)

summary(model)
anova(model) 


# Step 2: automatic stepwise selection to identify best variable
#step_result <- step(model)
#step_result

# Step 3: extract best model
#final_model <- get_model(step_result)
#summary(final_model)

# Step 2: manual stepwise selection by backwards elimination based on AIC
model1 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 temp_z : do_z +
                 temp_z : cond_z +
                 do_z : cond_z +
                 (1 | year), 
               data = departure_spawning,
               REML = FALSE)

model2 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 #temp_z : do_z +
                 temp_z : cond_z +
                 do_z : cond_z +
                 (1 | year), 
               data = departure_spawning,
               REML = FALSE)

model3 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 #temp_z : do_z +
                 #temp_z : cond_z +
                 do_z : cond_z +
                 (1 | year), 
               data = departure_spawning,
               REML = FALSE)

model4 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 #temp_z : do_z +
                 #temp_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = departure_spawning,
               REML = FALSE)

model5 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 #sex +
                 #temp_z : do_z +
                 #temp_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = departure_spawning,
               REML = FALSE)

model6 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 #cond_z + 
                 #sex +
                 #temp_z : do_z +
                 #temp_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = departure_spawning,
               REML = FALSE)

model7 <- lmer(daynumber ~ temp_z + 
                 #do_z + 
                 #cond_z + 
                 #sex +
                 #temp_z : do_z +
                 #temp_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = departure_spawning,
               REML = FALSE)

AIC(model6, model7)

summary(model7)

# Step 3: Identify best model
final_model <- lmer(daynumber ~ temp_z +
                      (1 | year), 
                    data = departure_spawning #,
                    #REML = FALSE   # Make sure to use REML again when running the final model
)

summary(final_model)


# Create table with model output
table_data <- tidy(final_model)

# Step 4: check model performance and validation
# Marginal R²: variance explained by fixed effects
# Conditional R²: variance explained by the total model
performance::r2(final_model)

plot(final_model) 

qqnorm(residuals(final_model))
qqline(residuals(final_model))





# 8. Environmental conditions at end of downstream migration ####
# Filter records of end downstream migration
end_downstream <- downstream %>%
  group_by(tag_serial_number) %>%
  filter(departure == max(departure, na.rm = TRUE)) %>%
  ungroup()


# Create exploratory boxplot
ggplot(end_downstream, aes(x=year, y=temp_at_arrival)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Variable") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


# Set starting date as day of the year (= day number)
end_downstream$daynumber <- yday(end_downstream$arrival)

# Explorative boxplot
ggplot(end_downstream, aes(x = year, y = daynumber)) +
  geom_boxplot() +
  ylab("Day of the year") + 
  xlab("Year") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")

# Analyse difference in arrival at spawning between years in relation to water temperature
year_summary <- end_downstream %>%
  group_by(year) %>%
  summarise(
    mean_daynumber = mean(daynumber)# of de waarde die bij dat jaar hoort
  )

year_summary <- left_join(year_summary, march_temp, by = "year")

ggplot(year_summary, aes(x = mean_temp_month, y = mean_daynumber)) +
  geom_point(size = 4, color = "black") +
  geom_text(aes(label = year), vjust = -1) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
  labs(#title = "Annual deviation vs average spring temperature",
    x = "Average March temperature (°C)",
    y = "Average day to end downstream migration") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")

# Run linear model
model <- lm(mean_daynumber ~ mean_temp_month, data = year_summary)
summary(model)

par(mfrow = c(2, 2))
plot(model)



# Apply GLMM

# Check number of shads per year
end_downstream %>%
  group_by(year) %>%
  summarise(n = n_distinct(tag_serial_number))

# Check locations
unique(end_downstream$location)

# Remove end location as zeeschelde because these are shads that were missed in the Westerschelde, but had ended their downstream migration likely later
#end_downstream <- filter(end_downstream, location != "zeeschelde")
# => This removes 2 shads


# Scale variables
end_downstream <- end_downstream %>%
  mutate(
    temp_z      = as.numeric(scale(temp_at_arrival)),
    do_z  = as.numeric(scale(do_at_arrival)),
    cond_z      = as.numeric(scale(cond_at_arrival)),
    turb_z      = as.numeric(scale(turb_at_arrival))
  ) 


# Step 1: Conduct GLMM
model <- lmer(daynumber ~ temp_z + do_z + cond_z + sex + #turb_z +
                temp_z : do_z +
                temp_z : cond_z +
                do_z : cond_z #+
              #temp_z : turb_z +
              #do_z : turb_z +
              #cond_z : turb_z
              + (1 | year), 
              data = end_downstream)

summary(model)
anova(model) 


# Step 2: automatic stepwise selection to identify best variable
#step_result <- step(model)
#step_result

# Step 3: extract best model
#final_model <- get_model(step_result)
#summary(final_model)

# Step 2: manual stepwise selection by backwards elimination based on AIC
model1 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 temp_z : do_z +
                 temp_z : cond_z +
                 do_z : cond_z +
                 (1 | year), 
               data = end_downstream,
               REML = FALSE)

model2 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 temp_z : do_z +
                 temp_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = end_downstream,
               REML = FALSE)

model3 <- lmer(daynumber ~ temp_z + 
                 do_z + 
                 cond_z + 
                 sex +
                 #temp_z : do_z +
                 temp_z : cond_z +
                 #do_z : cond_z +
                 (1 | year), 
               data = end_downstream,
               REML = FALSE)


AIC(model2, model3)

summary(model2)

# Step 3: Identify best model
final_model <- lmer(daynumber ~ temp_z + 
                      do_z + 
                      cond_z + 
                      sex +
                      temp_z : do_z +
                      temp_z : cond_z +
                      (1 | year), 
                    data = end_downstream #,
                    #REML = FALSE   # Make sure to use REML again when running the final model
)

summary(final_model)


# Create table with model output
table_data <- tidy(final_model)

# Step 4: check model performance and validation
# Marginal R²: variance explained by fixed effects
# Conditional R²: variance explained by the total model
performance::r2(final_model)

plot(final_model) 

qqnorm(residuals(final_model))
qqline(residuals(final_model))









