# Analyse the size of the spawning area
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages ####
library(nlme)


# Source
source("./src/clean_spawning.R")


# 1. Filter spawning behaviour ####
spawning <- filter(data, behaviour == "spawning")

# Add column year
spawning$year <- factor(year(spawning$arrival))

# 2. Calculate extent of spawning area ####
spawning_area_size <- spawning %>%
  group_by(tag_serial_number, length1, sex, year) %>%
  summarize(
    coverage = (max(distance_to_source_m, na.rm = TRUE) - min(distance_to_source_m, na.rm = TRUE))/1000 # in km
  )


# 50% core use
core_area50<- spawning %>%
  group_by(tag_serial_number) %>%
  summarise(
    pos_downstream = quantile(distance_to_source_m, probs = 0.25, na.rm = TRUE),
    pos_upstream = quantile(distance_to_source_m, probs = 0.75, na.rm = TRUE),
    core_range_m_50 = pos_upstream - pos_downstream,
  )

# 70% core use
core_area70<- spawning %>%
  group_by(tag_serial_number) %>%
  summarise(
    pos_downstream = quantile(distance_to_source_m, probs = 0.15, na.rm = TRUE),
    pos_upstream = quantile(distance_to_source_m, probs = 0.85, na.rm = TRUE),
    core_range_m_70 = pos_upstream - pos_downstream,
  )

# 90% core use
core_area90<- spawning %>%
  group_by(tag_serial_number) %>%
  summarise(
    pos_downstream = quantile(distance_to_source_m, probs = 0.05, na.rm = TRUE),
    pos_upstream = quantile(distance_to_source_m, probs = 0.95, na.rm = TRUE),
    core_range_m_90 = pos_upstream - pos_downstream,
  )

# Join them together
spawning_area_size <- left_join(spawning_area_size, core_area50, by = "tag_serial_number")
spawning_area_size <- left_join(spawning_area_size, core_area70, by = "tag_serial_number")
spawning_area_size <- left_join(spawning_area_size, core_area90, by = "tag_serial_number")

spawning_area_size$core_range_m_50 <- spawning_area_size$core_range_m_50/1000
spawning_area_size$core_range_m_70 <- spawning_area_size$core_range_m_70/1000
spawning_area_size$core_range_m_90 <- spawning_area_size$core_range_m_90/1000

spawning_area_size <- select(spawning_area_size, tag_serial_number, length1, sex, year, coverage, core_range_m_50, , core_range_m_70, core_range_m_90)


# Check which stations the shads used during the 50% core use of the area
shads_core_detections <- spawning %>%
  group_by(tag_serial_number) %>%
  mutate(
    q25 = quantile(distance_to_source_m, 0.25, na.rm = TRUE),
    q75 = quantile(distance_to_source_m, 0.75, na.rm = TRUE)
  ) %>%
  filter(distance_to_source_m >= q25 & distance_to_source_m <= q75) %>%
  ungroup()

shads_core_detections <- filter(shads_core_detections, tag_serial_number != "1367863") # Considered outlier
unique(shads_core_detections$station_name)



# Calculate summaries
#spawning_area_size <- filter(spawning_area_size, tag_serial_number != "1367863") # Considered outlier

summary(spawning_area_size$coverage)
aggregate(spawning_area_size$coverage, list(spawning_area_size$sex), mean)

summary(spawning_area_size$core_range_m_50)
aggregate(spawning_area_size$core_range_m_50, list(spawning_area_size$sex), mean)

summary(spawning_area_size$core_range_m_90)
aggregate(spawning_area_size$core_range_m_90, list(spawning_area_size$sex), mean)

# 3. Visualisation via plots
# 3.2. Create boxplot for difference in sex ####
ggplot(spawning_area_size, aes(x=sex, y=coverage)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Spawning area size (km)") + 
  xlab("Sex") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")

# 3.3. Create dot plot for relation with length ####
ggplot(spawning_area_size, aes(x= length1, y=coverage, color = sex)) + 
  geom_point() +
  ylab("Spawning area size (km)") + 
  xlab("Total length (mm)") +
  scale_color_manual(values = c("M" = "blue",
                                "F" = "darkgreen")) +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 12, colour = "black", angle=360),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.title.y = element_text(size = 12)) +
  #scale_x_continuous(breaks = seq(0, 365, by = 30)) +
  geom_smooth(method='lm', se = F) +
  geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", linewidth = 1.5)




# 4. Statistical analysis ####
lmm <- lm(coverage ~ length1 + sex, data = spawning_area_size)
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
tapply(spawning_area_size$core_range_m_90, spawning_area_size$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(core_range_m_90 ~ sex, data = spawning_area_size)

# Conduct two sample t-test with equal variances
ttest <- t.test(core_range_m_90 ~ sex, data = spawning_area_size, var.equal = TRUE)
ttest

# Conduct Mann-Whitney U test (also called Wilcoxon Rank Sum test).
wilcox.test(core_range_m_50 ~ sex, data = spawning_area_size)

# Conduct anova to test for differences between years
model <- aov(core_range_m_50 ~ year, data = spawning_area_size)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc

# Conduct Kruskal-Wallis test
kruskal.test(core_range_m_90 ~ year, data = spawning_area_size)




# 5. Summarising boxplot with both complete coverage, 90% core use and 50% core use ####
speed_upstream$behaviour <- "upstream"
speed_downstream$behaviour <- "downstream"

full_cover <- select(spawning_area_size, tag_serial_number, sex, year, coverage)
core90 <- select(spawning_area_size, tag_serial_number, sex, year, core_range_m_90)
core50 <- select(spawning_area_size, tag_serial_number, sex, year, core_range_m_50)

full_cover$cover_type <- "Full range"
core90$cover_type <- "90% core use"
core50$cover_type <- "50% core use"

full_cover <- rename(full_cover, range = coverage)
core90 <- rename(core90, range = core_range_m_90)
core50 <- rename(core50, range = core_range_m_50)

total <- rbind(full_cover, core90)
total <- rbind(total, core50)

total$cover_type <- factor(total$cover_type, ordered = TRUE, 
                          levels = c("50% core use",
                                     "90% core use",
                                     "Full range"))


ggplot(total, aes(x=cover_type, y=range)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Spawning area size (km)") + 
  xlab("Area use") +
  #stat_summary(fun = "mean", geom = "point", #shape = 8,
  #             size = 4, color = "blue", show.legend = FALSE) +
  scale_y_continuous(breaks = seq(0, 150, by = 10)) +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=360),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.position = "none")


