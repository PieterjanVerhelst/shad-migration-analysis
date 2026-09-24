# Analyse the duration of the upstream migration, spawning itself and downstream migration
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




# 6. Duration upstream migration ####
# 6.1. Calculate duration ####
# = arrival first upstream till arrival first spawning
duration_upstream <- rbind(start_upstream, arrival_spawning)
duration_upstream <- duration_upstream %>%
  group_by(tag_serial_number, sex, length1) %>%
  summarise(
    start = min(arrival, na.rm = TRUE),
    stop = max(arrival, na.rm = TRUE),
    duration = difftime(stop, start, units = "days")) # of "secs", "hours", "days"
duration_upstream$year <- factor(year(duration_upstream$start))

# Calculate summaries
summary(duration_upstream$duration)
aggregate(duration_upstream$duration, list(duration_upstream$sex), mean)

# 6.2. Create boxplot for difference in sex ####
ggplot(duration_upstream, aes(x=sex, y=duration)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Duration (days)") + 
  xlab("Sex") +
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

# 6.3. Create dot plot for relation with length ####
ggplot(duration_upstream, aes(x= length1, y=duration, color = sex)) + 
  geom_point() +
  ylab("Duration (days)") + 
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
  geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5)


# 6.4. Statistical analysis ####
duration_upstream$duration <- as.numeric(duration_upstream$duration)

lmm <- lm(duration ~ length1 + sex, data = duration_upstream)
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
tapply(duration_upstream$duration, duration_upstream$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(duration ~ sex, data = duration_upstream)

# Conduct two sample t-test with equal variances
ttest <- t.test(duration ~ sex, data = duration_upstream, var.equal = TRUE)
ttest

# Conduct anova to test for differences between years
model <- aov(duration ~ year, data = duration_upstream)
summary(model)





# 7. Duration spawning ####
# 7.1. Calculate duration ####
# = arrival first spawning till departure last spawning
duration_spawning <- spawning %>%
  group_by(tag_serial_number, sex, length1) %>%
  summarise(
    start = min(arrival, na.rm = TRUE),
    stop = max(departure, na.rm = TRUE),
    duration = difftime(stop, start, units = "days")) # of "secs", "hours", "days"
duration_spawning$year <- factor(year(duration_spawning$start))

# Calculate summaries
summary(duration_spawning$duration)
aggregate(duration_spawning$duration, list(duration_spawning$sex), mean)

# 7.2. Create boxplot for difference in sex ####
ggplot(duration_spawning, aes(x=sex, y=duration)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Duration (days)") + 
  xlab("Sex") +
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

# 7.3. Create dot plot for relation with length ####
ggplot(duration_spawning, aes(x= length1, y=duration, color = sex)) + 
  geom_point() +
  ylab("Duration (days)") + 
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
  geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5)


# 7.4. Statistical analysis ####
duration_spawning$duration <- as.numeric(duration_spawning$duration)

lmm <- lm(duration ~ length1 + sex, data = duration_spawning)
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
tapply(duration_spawning$duration, duration_spawning$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(duration ~ sex, data = duration_spawning)

# Conduct two sample t-test with equal variances
ttest <- t.test(duration ~ sex, data = duration_spawning, var.equal = TRUE)
ttest

# Conduct anova to test for differences between years
model <- aov(duration ~ year, data = duration_spawning)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc



# 8. Duration downstream migration ####
# 8.1. Calculate duration ####
# = departure last spawning till departure last downstream migration
duration_downstream <- rbind(departure_spawning, end_downstream)
duration_downstream <- duration_downstream %>%
  group_by(tag_serial_number, sex, length1) %>%
  summarise(
    start = min(arrival, na.rm = TRUE),
    stop = max(arrival, na.rm = TRUE),
    duration = difftime(stop, start, units = "days")) # of "secs", "hours", "days"
duration_downstream$year <- factor(year(duration_downstream$start))

# Calculate summaries
summary(duration_downstream$duration)
aggregate(duration_downstream$duration, list(duration_downstream$sex), mean)


# 8.2. Create boxplot for difference in sex ####
ggplot(duration_downstream, aes(x=sex, y=duration)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Duration (days)") + 
  xlab("Sex") +
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

# 8.3. Create dot plot for relation with length ####
ggplot(duration_downstream, aes(x= length1, y=duration, color = sex)) + 
  geom_point() +
  ylab("Duration (days)") + 
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
  geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5)


# 8.4. Statistical analysis ####
duration_downstream$duration <- as.numeric(duration_downstream$duration)

lmm <- lm(duration ~ length1 + sex, data = duration_downstream)
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
tapply(duration_downstream$duration, duration_downstream$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(duration ~ sex, data = duration_downstream)

# Conduct two sample t-test with equal variances
ttest <- t.test(duration ~ sex, data = duration_downstream, var.equal = TRUE)
ttest


# Conduct anova to test for differences between years
model <- aov(duration ~ year, data = duration_downstream)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc



# 9. Summarising boxplot with all three durations ####
duration_upstream$behaviour <- "upstream"
duration_spawning$behaviour <- "spawning"
duration_downstream$behaviour <- "downstream"

total <- rbind(duration_upstream, duration_spawning)
total <- rbind(total, duration_downstream)

total$behaviour <- factor(total$behaviour, ordered = TRUE, 
                     levels = c("upstream",
                                "spawning",
                                "downstream"))

# Create boxplot 
ggplot(total, aes(x=behaviour, y=duration, fill = sex)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Duration (days)") + 
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
    axis.title.y = element_text(size = 16)#,
    #legend.position = "none"
    )
