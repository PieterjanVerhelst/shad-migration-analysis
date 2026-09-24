# Visualise shad activity in function of tide
# By Hanna Jaspaert
# hanna.jaspaert@ugent.be

# Source configuration
source("./src/env_variables/config.R")

# Load data
data_tij <- read.csv("./data/interim/data_with_env_variables.csv")

# circle diagram of arrival times in function of time after high water
data_tij_filter <- data_tij %>% filter(!is.na(downstream))
data_tij_filter <- filter(data_tij_filter, behaviour != "spawning")

data_tij_filter_up <- filter(data_tij_filter, behaviour == "upstream", downstream == "FALSE")
#data_tij_filter_up <- filter(data_tij_filter, behaviour == "upstream")
data_tij_filter_down <- filter(data_tij_filter, behaviour == "downstream", downstream == "TRUE")
#data_tij_filter_down <- filter(data_tij_filter, behaviour == "downstream")

data_tij_filter <- rbind(data_tij_filter_up, data_tij_filter_down)

p1 <- ggplot(data_tij_filter, aes(x = hour(tidetime_arr))) + #hier hoever van hoog en laag tij
  geom_bar(aes(fill = tij_at_arrival)) + #choose colors for fill
  coord_radial(r.axis.inside = TRUE, expand = FALSE) +
  scale_x_continuous(limits = c(0, 12), breaks = 0:12, expand = c(0, 0)) +
  labs(fill = "Tide:", y = element_blank(), x = "Hours after high water") +
  scale_fill_manual(values = c("ebb" = grey1, "flood" = grey2)) +
  style +
  theme(
    #axis.line = element_blank(),
    strip.text = element_text(size = 25),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey90"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)
  ) +
  annotate(
    "text",
    x = 12, # place at "north" outer edge
    y = max(table(hour(data_tij$tidetime_arr))) * 0.14, # halfway up radial axis
    label = "# obs",
    angle = 90, # vertical orientation
    hjust = 1.2,
    vjust = 1.2,
    size = 11
  ) + 
  facet_wrap(
    ~downstream, 
    scales = "free",
    labeller = labeller(downstream = c(
      "TRUE" = "Downstream migration",
      "FALSE" = "Upstream migration",
      "NA" = "No movement"
    ))
  ) # separate plots for downstream and upstream migration
print(p1)
#save png
#ggsave("./figures/environmental_variables/tide.png", p1, width = 10, height = 10, units = "in", dpi = 300)