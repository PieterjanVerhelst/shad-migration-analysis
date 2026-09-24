# Analyse the period of the upstream migration, spawning itself and downstream migration
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




# 6. Start of upstream migration ####
# Identify day number of the year based on arrival date
start_upstream$daynumber <- yday(start_upstream$arrival)
start_upstream$daynumber <- factor(start_upstream$daynumber)
start_upstream$year <- factor(year(start_upstream$arrival))

# Calculate summary
start_upstream_summary <- start_upstream %>%
  group_by(sex,
           daynumber) %>%
  #group_by(daynumber) %>%
  count()


# 6.1. Violin plot with start of upstream migration in relation to sex ####
# Create plot with means to show on plot
start_upstream_summary$sex <- factor(start_upstream_summary$sex)
start_upstream_summary$daynumber <- as.character(start_upstream_summary$daynumber)
start_upstream_summary$daynumber <- as.numeric(start_upstream_summary$daynumber)


summary(start_upstream_summary$daynumber)
means <- aggregate(start_upstream_summary$daynumber, list(start_upstream_summary$sex), mean)
means <- rename(means, mean_daynumber = "x",
                sex = "Group.1")
means$mean_daynumber <- round(means$mean_daynumber, digits = 0)


# Create actual plot
ggplot(start_upstream_summary, aes(x=sex, y=daynumber)) +
  #geom_boxplot() +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14)) +
  scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136,143,150,157,164), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May", "23 May", "30 May", "6 June", "13 June")) +
  
  geom_hline(yintercept = 80, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 87, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 94, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 101, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 108, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 115, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 122, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 129, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 136, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 143, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 150, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 157, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 164, linetype="dashed", color = "grey", linewidth=0.7) +
  
  geom_violin(width = 0.5, position=position_dodge(1)) +
  # scale_fill_manual(values=c("blue",
  #                           "#33FFFF",
  #                           "yellow",
  #                           "orange",
  #                           "red")) +
  ylab("Day of the year") + 
  xlab("Sex") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 0.85),
               show.legend = FALSE) +
  #  geom_text(data = means, aes(label = mean_daynumber, y = 390), size = 6, position = position_dodge(0.85)) +
  guides(fill=guide_legend(title="Sex")) #+
#coord_flip()



# 6.2. Dotplot with arrival at spawning grounds in relation to length ####
ggplot(start_upstream, aes(x= length1, y=daynumber, 
                             color=sex
)) + 
  geom_point() +
  scale_color_manual(values = c("M" = "blue",
                                "F" = "darkgreen")) +
  ylab("Day of the year") + 
  xlab("Total length (mm)") +
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
    axis.title.y = element_text(size = 12)) #+
#scale_x_continuous(breaks = seq(0, 365, by = 30)) +
# scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May")) +
#geom_smooth(method='lm', se = F) +
#geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5) +
#coord_flip()



# 6.3. Statistical analysis ####
# Apply linear mixed effects model
# Full model

start_upstream$daynumber <- as.character(start_upstream$daynumber)
start_upstream$daynumber <- as.numeric(start_upstream$daynumber)
start_upstream$sex <- as.factor(start_upstream$sex)

lmm <- lm(daynumber ~ length1 + sex, data = start_upstream)
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
tapply(start_upstream$daynumber, start_upstream$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(daynumber ~ sex, data = start_upstream)

# Conduct two sample t-test with equal variances
ttest <- t.test(daynumber ~ sex, data = start_upstream, var.equal = TRUE)
ttest


# Conduct anova to test for differences between years
model <- aov(daynumber ~ year, data = start_upstream)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc


# 7. Arrival at the spawning grounds ####
# Identify day number of the year based on arrival date
arrival_spawning$daynumber <- yday(arrival_spawning$arrival)
arrival_spawning$daynumber <- factor(arrival_spawning$daynumber)
arrival_spawning$year <- factor(year(arrival_spawning$arrival))

# Calculate summary
arrival_spawning_summary <- arrival_spawning %>%
  group_by(sex,
           daynumber) %>%
  #group_by(daynumber) %>%
  count()


# 7.1. Violin plot with arrival at spawning grounds in relation to sex ####
# Create plot with means to show on plot
arrival_spawning_summary$sex <- factor(arrival_spawning_summary$sex)
arrival_spawning_summary$daynumber <- as.character(arrival_spawning_summary$daynumber)
arrival_spawning_summary$daynumber <- as.numeric(arrival_spawning_summary$daynumber)

summary(arrival_spawning_summary$daynumber)
means <- aggregate(arrival_spawning_summary$daynumber, list(arrival_spawning_summary$sex), mean)
means <- rename(means, mean_daynumber = "x",
                sex = "Group.1")
means$mean_daynumber <- round(means$mean_daynumber, digits = 0)


# Create actual plot
ggplot(arrival_spawning_summary, aes(x=sex, y=daynumber)) +
  #geom_boxplot() +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14)) +
  scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136,143,150,157,164), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May", "23 May", "30 May", "6 June", "13 June")) +
  
  geom_hline(yintercept = 80, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 87, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 94, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 101, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 108, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 115, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 122, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 129, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 136, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 143, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 150, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 157, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 164, linetype="dashed", color = "grey", linewidth=0.7) +
  
  geom_violin(width = 0.5, position=position_dodge(1)) +
  # scale_fill_manual(values=c("blue",
  #                           "#33FFFF",
  #                           "yellow",
  #                           "orange",
  #                           "red")) +
  ylab("Day of the year") + 
  xlab("Sex") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 0.85),
               show.legend = FALSE) +
  #  geom_text(data = means, aes(label = mean_daynumber, y = 390), size = 6, position = position_dodge(0.85)) +
  guides(fill=guide_legend(title="Sex")) #+
  #coord_flip()



# 7.2. Dotplot with arrival at spawning grounds in relation to length ####
ggplot(arrival_spawning, aes(x= length1, y=daynumber, 
                             color=sex
)) + 
  geom_point() +
  scale_color_manual(values = c("M" = "blue",
                                "F" = "darkgreen")) +
  ylab("Day of the year") + 
  xlab("Total length (mm)") +
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
    axis.title.y = element_text(size = 12)) #+
  #scale_x_continuous(breaks = seq(0, 365, by = 30)) +
 # scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May")) +
  #geom_smooth(method='lm', se = F) +
  #geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5) +
  #coord_flip()



# 7.3. Statistical analysis ####
# Apply linear mixed effects model
# Full model

arrival_spawning$daynumber <- as.character(arrival_spawning$daynumber)
arrival_spawning$daynumber <- as.numeric(arrival_spawning$daynumber)
arrival_spawning$sex <- as.factor(arrival_spawning$sex)

lmm <- lm(daynumber ~ length1 + sex, data = arrival_spawning)
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
tapply(arrival_spawning$daynumber, arrival_spawning$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(daynumber ~ sex, data = arrival_spawning)

# Conduct two sample t-test with equal variances
ttest <- t.test(daynumber ~ sex, data = arrival_spawning, var.equal = TRUE)
ttest


# Conduct anova to test for differences between years
model <- aov(daynumber ~ year, data = arrival_spawning)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc


# 8. Departure at the spawning grounds ####
# Identify day number of the year based on departure date
departure_spawning$daynumber <- yday(departure_spawning$departure)
departure_spawning$daynumber <- factor(departure_spawning$daynumber)
departure_spawning$year <- factor(year(departure_spawning$arrival))

# Calculate summary
departure_spawning_summary <- departure_spawning %>%
  group_by(sex,
           daynumber) %>%
  #group_by(daynumber) %>%
  count()


# 8.1. Violin plot with arrival at spawning grounds in relation to sex ####
# Create plot with means to show on plot
departure_spawning_summary$sex <- factor(departure_spawning_summary$sex)
departure_spawning_summary$daynumber <- as.character(departure_spawning_summary$daynumber)
departure_spawning_summary$daynumber <- as.numeric(departure_spawning_summary$daynumber)

summary(departure_spawning_summary$daynumber)
means <- aggregate(departure_spawning_summary$daynumber, list(departure_spawning_summary$sex), mean)
means <- rename(means, mean_daynumber = "x",
                sex = "Group.1")
means$mean_daynumber <- round(means$mean_daynumber, digits = 0)


# Create actual plot
ggplot(departure_spawning_summary, aes(x=sex, y=daynumber)) +
  #geom_boxplot() +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14)) +
  scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136,143,150,157,164), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May", "23 May", "30 May", "6 June", "13 June")) +
  
  geom_hline(yintercept = 80, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 87, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 94, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 101, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 108, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 115, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 122, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 129, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 136, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 143, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 150, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 157, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 164, linetype="dashed", color = "grey", linewidth=0.7) +
  
  geom_violin(width = 0.5, position=position_dodge(1)) +
  # scale_fill_manual(values=c("blue",
  #                           "#33FFFF",
  #                           "yellow",
  #                           "orange",
  #                           "red")) +
  ylab("Day of the year") + 
  xlab("Sex") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 0.85),
               show.legend = FALSE) +
  #  geom_text(data = means, aes(label = mean_daynumber, y = 390), size = 6, position = position_dodge(0.85)) +
  guides(fill=guide_legend(title="Sex")) #+
#coord_flip()



# 8.2. Dotplot with arrival at spawning grounds in relation to length ####
ggplot(departure_spawning, aes(x= length1, y=daynumber, 
                             color=sex
)) + 
  geom_point() +
  scale_color_manual(values = c("M" = "blue",
                                "F" = "darkgreen")) +
  ylab("Day of the year") + 
  xlab("Total length (mm)") +
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
    axis.title.y = element_text(size = 12)) #+
#scale_x_continuous(breaks = seq(0, 365, by = 30)) +
# scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May")) +
#geom_smooth(method='lm', se = F) +
#geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5) +
#coord_flip()



# 8.3. Statistical analysis ####
# Apply linear mixed effects model
# Full model

departure_spawning$daynumber <- as.character(departure_spawning$daynumber)
departure_spawning$daynumber <- as.numeric(departure_spawning$daynumber)
departure_spawning$sex <- as.factor(departure_spawning$sex)

lmm <- lm(daynumber ~ length1 + sex, data = departure_spawning)
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
tapply(departure_spawning$daynumber, departure_spawning$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(daynumber ~ sex, data = departure_spawning)

# Conduct two sample t-test with equal variances
ttest <- t.test(daynumber ~ sex, data = departure_spawning, var.equal = TRUE)
ttest

# Conduct anova to test for differences between years
model <- aov(daynumber ~ year, data = departure_spawning)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc




# 9. End of downstream migration ####
# Identify day number of the year based on departure date
end_downstream$daynumber <- yday(end_downstream$departure)
end_downstream$daynumber <- factor(end_downstream$daynumber)
end_downstream$year <- factor(year(end_downstream$arrival))

# Calculate summary
end_downstream_summary <- end_downstream %>%
  group_by(sex,
           daynumber) %>%
  #group_by(daynumber) %>%
  count()


# 9.1. Violin plot with arrival at spawning grounds in relation to sex ####
# Create plot with means to show on plot
end_downstream_summary$sex <- factor(end_downstream_summary$sex)
end_downstream_summary$daynumber <- as.character(end_downstream_summary$daynumber)
end_downstream_summary$daynumber <- as.numeric(end_downstream_summary$daynumber)

summary(end_downstream_summary$daynumber)
means <- aggregate(end_downstream_summary$daynumber, list(end_downstream_summary$sex), mean)
means <- rename(means, mean_daynumber = "x",
                sex = "Group.1")
means$mean_daynumber <- round(means$mean_daynumber, digits = 0)


# Create actual plot
ggplot(end_downstream_summary, aes(x=sex, y=daynumber)) +
  #geom_boxplot() +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14)) +
  scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136,143,150,157,164), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May", "23 May", "30 May", "6 June", "13 June")) +
  
  geom_hline(yintercept = 80, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 87, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 94, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 101, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 108, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 115, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 122, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 129, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 136, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 143, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 150, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 157, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 164, linetype="dashed", color = "grey", linewidth=0.7) +
  
  geom_violin(width = 0.5, position=position_dodge(1)) +
  # scale_fill_manual(values=c("blue",
  #                           "#33FFFF",
  #                           "yellow",
  #                           "orange",
  #                           "red")) +
  ylab("Day of the year") + 
  xlab("Sex") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 0.85),
               show.legend = FALSE) +
  #  geom_text(data = means, aes(label = mean_daynumber, y = 390), size = 6, position = position_dodge(0.85)) +
  guides(fill=guide_legend(title="Sex")) #+
#coord_flip()



# 9.2. Dotplot with arrival at spawning grounds in relation to length ####
ggplot(end_downstream, aes(x= length1, y=daynumber, 
                               color=sex
)) + 
  geom_point() +
  scale_color_manual(values = c("M" = "blue",
                                "F" = "darkgreen")) +
  ylab("Day of the year") + 
  xlab("Total length (mm)") +
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
    axis.title.y = element_text(size = 12)) #+
#scale_x_continuous(breaks = seq(0, 365, by = 30)) +
# scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May")) +
#geom_smooth(method='lm', se = F) +
#geom_smooth(method='lm', se = F, aes(group = 1), colour = "black", size = 1.5) +
#coord_flip()



# 9.3. Statistical analysis ####
# Apply linear mixed effects model
# Full model

end_downstream$daynumber <- as.character(end_downstream$daynumber)
end_downstream$daynumber <- as.numeric(end_downstream$daynumber)
end_downstream$sex <- as.factor(end_downstream$sex)

lmm <- lm(daynumber ~ length1 + sex, data = end_downstream)
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
tapply(end_downstream$daynumber, end_downstream$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(daynumber ~ sex, data = end_downstream)

# Conduct two sample t-test with equal variances
ttest <- t.test(daynumber ~ sex, data = end_downstream, var.equal = TRUE)
ttest

# Conduct anova to test for differences between years
model <- aov(daynumber ~ year, data = end_downstream)
summary(model)

post_hoc <- TukeyHSD(model)
post_hoc



# 10. Summarising violin plot with all four periods ####
start_upstream_summary$type <- "start_upstream"
arrival_spawning_summary$type <- "arrival_spawning"
departure_spawning_summary$type <- "departure_spawning"
end_downstream_summary$type <- "end_downstream"

total <- rbind(start_upstream_summary, arrival_spawning_summary)
total <- rbind(total, departure_spawning_summary)
total <- rbind(total, end_downstream_summary)

means <- aggregate(total$daynumber, list(total$sex, total$type), mean)
means <- rename(means, mean_daynumber = "x",
                sex = "Group.1",
                type = "Group.2")
means$mean_daynumber <- round(means$mean_daynumber, digits = 0)

total$type <- factor(total$type, ordered = TRUE, 
                    levels = c("start_upstream",
                               "arrival_spawning",
                               "departure_spawning",
                               "end_downstream"))

total <- mutate(total, type = fct_recode(type,
                                        "Start upstream" = "start_upstream",
                                        "Arrival spawning" = "arrival_spawning",
                                        "Departure spawning" = "departure_spawning",
                                        "End downstream" = "end_downstream"))

# Create actual plot
ggplot(total, aes(x=type, y=daynumber)) +
  #geom_boxplot() +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black", angle=0),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 14)) +
  scale_y_continuous(breaks = c(80,87,94,101,108,115,122,129,136,143,150,157,164), labels = c("21 March","28 March","4 April","11 April","18 April","25 April", "2 May", "9 May", "16 May", "23 May", "30 May", "6 June", "13 June")) +
  
  geom_hline(yintercept = 80, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 87, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 94, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 101, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 108, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 115, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 122, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 129, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 136, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 143, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 150, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 157, linetype="dashed", color = "grey", linewidth=0.7) +
  geom_hline(yintercept = 164, linetype="dashed", color = "grey", linewidth=0.7) +
  
  geom_violin(width = 0.5, position=position_dodge(1)) +
  # scale_fill_manual(values=c("blue",
  #                           "#33FFFF",
  #                           "yellow",
  #                           "orange",
  #                           "red")) +
  ylab("Day of the year") + 
  xlab("Behaviour") +
  stat_summary(fun = "mean", geom = "point", #shape = 8,
               size = 2, color = "black",
               position = position_dodge(width = 0.85),
               show.legend = FALSE) #+
  #  geom_text(data = means, aes(label = mean_daynumber, y = 390), size = 6, position = position_dodge(0.85)) +
  #guides(fill=guide_legend(title="Sex")) #+
#coord_flip()
