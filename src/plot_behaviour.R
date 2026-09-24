# Create distance plots with the different behaviour types
# Pieterjan Verhelst
# pieterjan.verhelst@inbo.be

# Load libraries
library(tidyverse)
library(lubridate)
library(tidyquant)

# Load data
data <- read_csv('./data/interim/data_with_behaviour_types.csv') 


# Create plot for a single shad
output_shad <- data %>% 
  filter(tag_serial_number == "1308531")

plot <- ggplot() + geom_line(aes(arrival, -1*distance_to_source_m), data = output_shad, colour = "black", size = 1) + 
  geom_point(aes(arrival, -1*distance_to_source_m, colour = behaviour), data = output_shad, shape = 1, size = 5) + 
  #ggtitle(data2$tag_serial_number) +  
  labs(title = output_shad$tag_serial_number, subtitle = output_shad$catch_year) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=14)) + 
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
    axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, colour = "black"),
    axis.title.y = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 month") + 
  geom_hline(yintercept = -1*output_shad$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed") +
  annotate("text",x = output_shad$arrival[1] - (1000*60*60), y = -1*output_shad$distance_to_source_m, label = output_shad$station_name, hjust=0, colour="red", size = 3)

plot


# Create pdf with the plots of all returning shad
mydfnew.split.shad <- split(data, data$tag_serial_number) # split dataset based on tag IDs
pdf("./figures/distance_tracks_returners_behaviours_may2026.pdf") # Create pdf

for (i in 1:length(mydfnew.split.shad)){ #i van 1 tot aantal transmitters
  mydfnew.temp<-mydfnew.split.shad[[i]] #for loop wordt doorlopen voor elke i transmitter
  g <- ggplot()
  g <- g + theme(axis.text.x = element_text(size = 12, colour = "black", angle=90))
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))
  g <- g + geom_line(aes(arrival, (-1*distance_to_source_m)/1000), data = mydfnew.temp, colour = "black", size = 1)
  g <- g + geom_point(aes(arrival, (-1*distance_to_source_m)/1000, colour = behaviour), data = mydfnew.temp, shape = 1, size = 5)
  g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
  #g <- g + scale_y_continuous(limit = c(-200000, 40000),
  #                            breaks = c(-200000,-190000,-180000,-170000,-160000,-150000,-140000,-130000,-120000,-110000,-100000,-90000,-80000,-70000,-60000,-50000,-40000,-30000,-20000,-10000,0,10000,20000,30000,40000), 
  #                            labels = c(-200,-190,-180,-170,-160,-150,-140,-130,-120,-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40))
  g <- g + labs(title = mydfnew.temp$tag_serial_number, subtitle = mydfnew.temp$catch_year) 
  g <- g + ylab("Distance to tagging location (km)")
  g <- g + xlab("Date")
  g <- g + theme(legend.position="bottom")
  g <- g + scale_x_datetime(date_breaks  ="1 month")
  g <- g + geom_hline(yintercept = (-1*mydfnew.temp$distance_to_source_m)/1000, colour = "gray", size = 0.5, linetype = "dashed")
  g <- g + annotate("text",x = mydfnew.temp$arrival[1]- (2400*60*60), y = (-1*mydfnew.temp$distance_to_source_m)/1000, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
  print(g)
}

dev.off()


# Create pdf with the spawning event only of all returning shad
data_spawning <- filter(data, behaviour == "upstream" |
                          behaviour == "spawning" |
                          behaviour == "downstream")


mydfnew.split.shad <- split(data_spawning, data_spawning$tag_serial_number) # split dataset based on tag IDs
pdf("./figures/distance_tracks_returners_spawning_may2026.pdf") # Create pdf

for (i in 1:length(mydfnew.split.shad)){ #i van 1 tot aantal transmitters
  mydfnew.temp<-mydfnew.split.shad[[i]] #for loop wordt doorlopen voor elke i transmitter
  g <- ggplot()
  g <- g + theme(axis.text.x = element_text(size = 12, colour = "black", angle=90))
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))
  g <- g + geom_line(aes(arrival, (-1*distance_to_source_m)/1000), data = mydfnew.temp, colour = "black", size = 1)
  g <- g + geom_point(aes(arrival, (-1*distance_to_source_m)/1000, colour = behaviour), data = mydfnew.temp, shape = 1, size = 5)
  g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=20))
  #g <- g + scale_y_continuous(limit = c(-200000, 40000),
  #                            breaks = c(-200000,-190000,-180000,-170000,-160000,-150000,-140000,-130000,-120000,-110000,-100000,-90000,-80000,-70000,-60000,-50000,-40000,-30000,-20000,-10000,0,10000,20000,30000,40000), 
  #                            labels = c(-200,-190,-180,-170,-160,-150,-140,-130,-120,-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40))
  g <- g + labs(title = mydfnew.temp$tag_serial_number) 
  g <- g + ylab("Distance to tagging location (km)")
  g <- g + xlab("Date")
  g <- g + theme(legend.position="bottom")
  g <- g + scale_x_datetime(date_breaks  ="1 week")
  g <- g + geom_hline(yintercept = (-1*mydfnew.temp$distance_to_source_m)/1000, colour = "gray", size = 0.5, linetype = "dashed")
 # g <- g + annotate("text",x = mydfnew.temp$arrival[1]- (2400*60*60), y = (-1*mydfnew.temp$distance_to_source_m)/1000, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
  print(g)
}

dev.off()
