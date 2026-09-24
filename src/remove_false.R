# Remove false detections on networks outside of Belgium
# These false detections have been evaluated by Jan Reubens (VLIZ), Dana Allen (InnovaSea) and myself (PJ; INBO)
# By Pieterjan Verhelst
# Pieterjan.Verhelst@inbo.be

unique(data$acoustic_project_code)
unique(data$station_name)

# Calculate tracking time to identify which transmitters were detected after their tag life
time_diff <- data %>%
  group_by(tag_serial_number) %>%
  summarize(duration = difftime(max(date_time), min(date_time), units = "days"))



data <- data[!(data$acoustic_project_code == "MOBEIA"),]
data <- data[!(data$tag_serial_number == "1367859" & data$station_name == "S2_B31_E"),]
data <- data[!(data$tag_serial_number == "1367865" & data$station_name == "22"),]
data <- data[!(data$tag_serial_number == "22084662" & data$station_name == "ROERMOND"),]
data <- data[!(data$tag_serial_number == "02CY" & data$station_name == "V7"),]

# Uncertainty on detection near Scotland
data <- data[!(data$acoustic_project_code == "PrePARED"),]

# Remove detections from detection stations in Mallorca
data <- data[!(data$acoustic_project_code == "BTN-IMEDEA"),]

# Remove detections from detection stations in Southern Spain
data <- data[!(data$acoustic_project_code == "FarmTrack"),]

# Remove detections from detection stations in Southern France
data <- data[!(data$acoustic_project_code == "CONNECT-MED"),]

# Remove detections from detection stations in Portugal
data <- data[!(data$acoustic_project_code == "BlueCrab2022Algarve"),]
data <- data[!(data$acoustic_project_code == "PTN/ATLAZUL"),]

# Remove detections from detection stations in Northern Spain
data <- data[!(data$acoustic_project_code == "GTN"),]

# Remove detections from detection stations in Denmark
data <- data[!(data$acoustic_project_code == "COD_OWF"),]
data <- data[!(data$acoustic_project_code == "Danish_Straits"),]
data <- data[!(data$acoustic_project_code == "Hevring_Trout_Denmark"),]

# Remove detections from detection stations in Sweden
data <- data[!(data$acoustic_project_code == "SEM"),]
data <- data[!(data$acoustic_project_code == "KBTN"),]

# Remove detections from Gibraltar Strait
data <- data[!(data$acoustic_project_code == "STRAITS_GIBRALTAR_ARRAY"),]

# Remove detections from Rhine at the Dutch-German border
data <- data[!(data$tag_serial_number == "05HZ" & data$station_name == "Stuw grens beneden Duitsland"),]
data <- data[!(data$tag_serial_number == "22084648" & data$station_name == "Stuw grens beneden Duitsland"),]

# Remove detections from detection stations in Norway
data <- data[!(data$acoustic_project_code == "SkagNor"),]

# Remove detections from detection station in Van Harinxmakanaal in the Netherlands
data <- data[!(data$acoustic_project_code == "2024_Anguilla_bb_Harlingen"),]

# Remove detections from detection station of project SPIDER_GNB_array
data <- data[!(data$acoustic_project_code == "SPIDER_GNB_array"),]

# Remove detection that falls after tag and date from the MMermaid network
data <- data[!(data$tag_serial_number == "22084660" & data$station_name == "FEC04"),]


# Remove detections that occurred outside battery life of transmitter
data <- data[!(data$tag_serial_number == "22084676" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084670" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084671" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084675" & data$date_time > "2024-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084677" & data$date_time > "2024-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084682" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084665" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084657" & data$date_time > "2024-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084649" & data$date_time > "2024-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "02CR" & data$date_time > "2024-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084648" & data$date_time > "2024-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "23087270" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "02D0" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "02CB" & data$date_time > "2025-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "1367842" & data$date_time > "2023-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "22084655" & data$date_time > "2024-01-01 00:00:00"),]
data <- data[!(data$tag_serial_number == "23087250" & data$date_time > "2025-01-01 00:00:00"),]





