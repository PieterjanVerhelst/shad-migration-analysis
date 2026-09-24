# Analyse the difference in size between fish that have been selected for the spawning migration analysis
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Source
source("./src/clean_spawning.R")

# Select the shads from the migration data
size <- select(data, tag_serial_number, sex, length1)
size <- distinct(size)
size$sex <- factor(size$sex)

# Create plot
ggplot(exp, aes(x=sex, y=length1)) + 
  geom_boxplot() +
  #scale_fill_manual(values = c("nontidal" = "white",
  #                             "tidal" = "lightgrey")) +
  ylab("Total length (mm)") + 
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


# Conduct two sample t-test

# Check normality (Shapiro-Wilk test)
# p > 0.05 suggests normal distribution
tapply(size$length1, size$sex, shapiro.test)

# Check homogeneity of variances (Levene's Test)
car::leveneTest(length1 ~ sex, data = size)

# Conduct two sample t-test with equal variances
ttest <- t.test(length1 ~ sex, data = size, var.equal = TRUE)
ttest
