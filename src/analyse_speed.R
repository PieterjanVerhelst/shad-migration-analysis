# Analyse the migration speed during upstream and downstream migration.
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be



# Load packages ####
library(nlme)


# Source
source("./src/clean_spawning.R")


# 1. Filter different behaviours ####
spawning <- filter(data, behaviour == "spawning")
upstream <- filter(data, behaviour == "upstream")
downstream <- filter(data, behaviour == "downstream")

# 2. Filter records of first spawning (= arrival spawning) ####
arrival_spawning <- spawning %>%
  group_by(tag_serial_number) %>%
  filter(arrival == min(arrival, na.rm = TRUE)) %>%
  ungroup()

# 3. Filter records of last spawning (= departure spawning) ####
departure_spawning <- spawning %>%
  group_by(tag_serial_number) %>%
  filter(departure == max(departure, na.rm = TRUE)) %>%
  ungroup()

# 4. Filter records of start upstream migration ####
start_upstream <- upstream %>%
  group_by(tag_serial_number) %>%
  filter(arrival == min(arrival, na.rm = TRUE)) %>%
  ungroup()

# 5. Filter records of end downstream migration ####
end_downstream <- downstream %>%
  group_by(tag_serial_number) %>%
  filter(departure == max(departure, na.rm = TRUE)) %>%
  ungroup()



# 6. Upstream migration speed ####
# 6.1. Calculate duration ####
# = arrival first upstream till arrival first spawning
duration_upstream <- rbind(start_upstream, arrival_spawning)
duration_upstream <- duration_upstream %>%
  group_by(tag_serial_number, sex, length1) %>%
  summarise(
    start = min(arrival, na.rm = TRUE),
    stop = max(arrival, na.rm = TRUE),
    duration = difftime(stop, start, units = "days")) # of "secs", "hours", "days"

# 6.2. Calculate distance ####
# = arrival first upstream till arrival first spawning
distance_upstream <- rbind(start_upstream, arrival_spawning)
distance_upstream <- distance_upstream %>%
  group_by(tag_serial_number) %>%
  summarize(
    coverage = (max(distance_to_source_m, na.rm = TRUE) - min(distance_to_source_m, na.rm = TRUE))/1000 # in km
  )

# 6.3. Calculate speed ####
speed_upstream <- left_join(duration_upstream, distance_upstream, by = "tag_serial_number")
speed_upstream$duration <- as.numeric(speed_upstream$duration)
speed_upstream$speed_km_day <- speed_upstream$coverage / speed_upstream$duration

# Add column year
speed_upstream$year <- factor(year(speed_upstream$start))

# Calculate summaries
summary(speed_upstream$speed_km_day)
aggregate(speed_upstream$speed_km_day, list(speed_upstream$sex), mean)

# 6.4 Statistical analysis ####
lmm <- lm(speed_km_day ~ length1 + sex, data = speed_upstream)
summary(lmm)

plot(lmm)
par(mfrow=c(2,2))
qqnorm(resid(lmm, type = "pearson"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(lmm, type = "pearson"))
plot(fitted(lmm),resid(lmm, type = "pearson"))
dev.off()


# Conduct two sample t-test to only test for difference between sex

# Check normality (Shapiro-Wilk test)
# p > 0.05 suggests normal distribution
tapply(speed_upstream$speed_km_day, speed_upstream$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(speed_km_day ~ sex, data = speed_upstream)

# Conduct two sample t-test with equal variances
ttest <- t.test(speed_km_day ~ sex, data = speed_upstream, var.equal = TRUE)
ttest


# Conduct anova to test for differences between years
model <- aov(speed_km_day ~ year, data = speed_upstream)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc







# 7. Downstream migration speed ####
# 7.1. Calculate duration ####
# = departure last spawning till departure last downstream migration
duration_downstream <- rbind(departure_spawning, end_downstream)
duration_downstream <- duration_downstream %>%
  group_by(tag_serial_number, sex, length1) %>%
  summarise(
    start = min(arrival, na.rm = TRUE),
    stop = max(arrival, na.rm = TRUE),
    duration = difftime(stop, start, units = "days")) # of "secs", "hours", "days"


# 7.2. Calculate distance ####
# = departure last spawning till departure last downstream migration
distance_downstream <- rbind(departure_spawning, end_downstream)
distance_downstream <- distance_downstream %>%
  group_by(tag_serial_number) %>%
  summarize(
    coverage = (max(distance_to_source_m, na.rm = TRUE) - min(distance_to_source_m, na.rm = TRUE))/1000 # in km
  )

# 7.3. Calculate speed ####
speed_downstream <- left_join(duration_downstream, distance_downstream, by = "tag_serial_number")
speed_downstream$duration <- as.numeric(speed_downstream$duration)
speed_downstream$speed_km_day <- speed_downstream$coverage / speed_downstream$duration

# Add column year
speed_downstream$year <- factor(year(speed_downstream$start))

# Calculate summaries
summary(speed_downstream$speed_km_day)
aggregate(speed_downstream$speed_km_day, list(speed_downstream$sex), mean)

# 7.4 Statistical analysis ####
lmm <- lm(speed_km_day ~ length1 + sex, data = speed_downstream)
summary(lmm)

plot(lmm)
par(mfrow=c(2,2))
qqnorm(resid(lmm, type = "pearson"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(lmm, type = "pearson"))
plot(fitted(lmm),resid(lmm, type = "pearson"))
dev.off()


# Conduct two sample t-test to only test for difference between sex

# Check normality (Shapiro-Wilk test)
# p > 0.05 suggests normal distribution
tapply(speed_downstream$speed_km_day, speed_downstream$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(speed_km_day ~ sex, data = speed_downstream)

# Conduct two sample t-test with equal variances
ttest <- t.test(speed_km_day ~ sex, data = speed_downstream, var.equal = TRUE)
ttest


# Conduct anova to test for differences between years
model <- aov(speed_km_day ~ year, data = speed_downstream)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc




# 8. Summarising boxplot with both upstream and downstream migration speeds ####
speed_upstream$behaviour <- "upstream"
speed_downstream$behaviour <- "downstream"

total <- rbind(speed_upstream, speed_downstream)

total$behaviour <- factor(total$behaviour, ordered = TRUE, 
                          levels = c("upstream",
                                     "downstream"))

# Create boxplot 
ggplot(total, aes(x=behaviour, y=speed_km_day)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Speed (km/day)") + 
  xlab("Behaviour") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=90),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")
