# Create plots with travelled distance and store as .pdf
# by Ine Pauwels, Pieterjan Verhelst & Stijn Bruneel
# ine.pauwels@inbo.be, Pieterjan.Verhelst@UGent.be, Stijn.Bruneel@ugent.be

library(tidyverse)
library(lubridate)
library(tidyquant)



# Source 
source("./src/merge_shad_characteristics2.R")

# Create plot for all eels in single pdf
#data$arrival <- ymd_hms(data$arrival)
#data$arrival <- as.POSIXct(strptime(data$arrival,"%Y-%m-%d %H:%M:%S"))


# Create plots for shads that were detected > 1 year and hence returned to the spawning grounds the year after tagging
#data <- filter(data, acoustic_tag_id == "A69-1601-64866" |
#                   acoustic_tag_id == "A69-1601-64868" |
#                   acoustic_tag_id == "A69-1602-12452" |
#                   acoustic_tag_id == "A69-1602-12455" |
#                   acoustic_tag_id == "A69-1602-12456" |
#                   acoustic_tag_id == "A69-1602-12458" |
#                   acoustic_tag_id == "A69-1602-12459" |
#                   acoustic_tag_id == "A69-1602-12461" |
#                   acoustic_tag_id == "A69-1602-12462" |
#                   acoustic_tag_id == "A69-1602-42556" |
#                   acoustic_tag_id == "A69-1602-42560" |
#                   acoustic_tag_id == "A69-1602-42562" |
#                   acoustic_tag_id == "A69-1602-42563" |
#                   acoustic_tag_id == "A69-1602-42564" |
#                   acoustic_tag_id == "A69-1602-42565" |
#                   acoustic_tag_id == "A69-1602-42570" |
#                   acoustic_tag_id == "A69-1602-42571" |
#                   acoustic_tag_id == "A69-1602-42576" |
#                  # acoustic_tag_id == "A69-1602-42578" |  # Lost tag or died in Westerschelde
#                   acoustic_tag_id == "A69-1602-42579" |
#                   acoustic_tag_id == "A69-1602-42583"
#)


data$tag_serial_number <- factor(data$tag_serial_number)
data$catch_year <- factor(data$catch_year)

data_all <- data
data <- filter(data_all, catch_year == "2024")
data$catch_year <- factor(data$catch_year)
data$tag_serial_number <- factor(data$tag_serial_number)


mydfnew.split.eel <- split(data, data$tag_serial_number) # split dataset based on tag IDs
pdf("./figures/distance_tracks_shads2024.pdf") # Create pdf


for (i in 1:length(mydfnew.split.eel)){ #i van 1 tot aantal transmitters
  mydfnew.temp<-mydfnew.split.eel[[i]] #for loop wordt doorlopen voor elke i transmitter
  g <- ggplot()
  g <- g + theme(axis.text.x = element_text(size = 12, colour = "black", angle=90))
  g <- g + geom_line(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, colour = "black", linewidth = 1)
  g <- g + geom_point(aes(arrival, -1*distance_to_source_m), data = mydfnew.temp, shape = 1, size = 5, colour = "black")
  g <- g + theme(plot.title = element_text(lineheight=.8, face="bold", size=14))
#  g <- g + scale_y_continuous(limit = c(-200000, 40000),
#                              breaks = c(-200000,-190000,-180000,-170000,-160000,-150000,-140000,-130000,-120000,-110000,-100000,-90000,-80000,-70000,-60000,-50000,-40000,-30000,-20000,-10000,0,10000,20000,30000,40000), 
#                              labels = c(-200,-190,-180,-170,-160,-150,-140,-130,-120,-110,-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40))
  g <- g + labs(title = mydfnew.temp$tag_serial_number, subtitle = mydfnew.temp$catch_year) 
  g <- g +   theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size = 14, colour = "black", angle=90),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, colour = "black"),
    axis.title.y = element_text(size = 14))
  g <- g + ylab("Distance (km)")
  g <- g + xlab("Date")
  g <- g + scale_x_datetime(date_breaks  ="1 month")
  g <- g + geom_hline(yintercept = -1*mydfnew.temp$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed")
  g <- g + annotate("text",x = mydfnew.temp$arrival[1]- (2400*60*60), y = -1*mydfnew.temp$distance_to_source_m, label = mydfnew.temp$station_name, hjust=0, colour="red", size = 3)
  print(g)
}

dev.off()




## Create single plot

# Select individual
data2 <- data[which(data$tag_serial_number == "1367869"), ]
#data2=data2[order(as.POSIXct(strptime(data2$Arrival,"%d/%m/%Y %H:%M"))),]
data2 <- data2[order(as.POSIXct(strptime(data2$arrival,"%Y-%m-%d %H:%M:%S"))),]

# Create plot
plot <- ggplot() + geom_line(aes(arrival, -1*distance_to_source_m), data = data2, colour = "black", size = 1) + 
  geom_point(aes(arrival, -1*distance_to_source_m), data = data2, shape = 1, size = 5, colour = "black") + 
  #ggtitle(data2$acoustic_tag_id) +  
  labs(title = data2$tag_serial_number, subtitle = data2$catch_year) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
  ylab("Distance (km)") + 
  xlab("Date") + 
  scale_y_continuous(limit = c(-400000, 40000),
                     breaks = c(-400000,-350000,-300000,-250000,-200000, -150000,-100000,-50000,0, 50000), 
                     labels = c(-400,-350,-300,-250,-200,-150,-100,-50,0,50)) +
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
  geom_hline(yintercept = -1*data2$distance_to_source_m, colour = "gray", size = 0.5, linetype = "dashed") +
  annotate("text",x = data2$arrival[1] - (1000*60*60), y = -1*data2$distance_to_source_m, label = data2$station_name, hjust=0, colour="red", size = 3)

plot


# Zoom to specific regions in plot
plot + coord_x_datetime(xlim = c("2020-05-05 00:00:00 UTC", "2020-05-12 00:00:00 UTC"), ylim= c(-10000,120000))



