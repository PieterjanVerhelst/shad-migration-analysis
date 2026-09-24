# Polish spawning migration behaviour
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# 1. Shad 1264425: end of upstream migration should be spawning ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1264425" & arrival > '2021-04-24 23:17:14' & arrival < '2021-05-01 19:15:29', 'spawning', migration))

# 2. Shad 1308531: downstream migration should end at most downstream station ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1308531" & arrival > '2021-06-09 00:00:00', NA, migration))


# 3. Shad 1367860: end of upstream migration should be spawning ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1367860" & arrival > '2022-04-08 18:58:11' & arrival < '2022-04-18 05:08:40', 'spawning', migration))


# 4. Shad 22084650: downstream migration not defined ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084650" & arrival > '2023-05-15 22:11:45' & arrival < '2023-06-13 23:29:32', 'downstream', migration))


# 5. Shad 22084653: first records in Westerschelde should be classified as upstream migration ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084653" & arrival > '2023-04-03 08:06:12' & arrival < '2023-04-26 15:23:07', 'upstream', migration))


# 6. Shad 22084653: one more record after final downstream migration should also be downstream migration ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084653" & arrival > '2023-05-18 14:00:00' & arrival < '2023-05-19 14:00:00', 'downstream', migration))


# 7. Shad 22084673: downstream migration not defined ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084673" & arrival > '2023-05-08 08:01:42' & arrival < '2023-05-23 14:45:28', 'downstream', migration))

data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084673" & arrival > '2023-05-23 14:45:28', NA, migration))


# 8. Shad 22084679: downstream migration not defined ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084679" & arrival > '2023-05-12 22:46:40' & arrival < '2023-06-11 00:00:00', 'downstream', migration))

data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084679" & arrival > '2023-06-07 00:00:00', NA, migration))

# 9. Shad 02D3: downstream migration should end at most downstream station ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "02D3" & arrival > '2024-05-22 12:29:03', NA, migration))


# 10. Shad 23087277: end of upstream migration should be spawning ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "23087277" & arrival > '2024-04-10 00:36:23' & arrival < '2024-04-12 19:01:40', 'spawning', migration))


# 11. Shad 23087277: start of upstream migration should start earlier ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "23087277" & arrival > '2024-04-06 07:25:46' & arrival < '2024-04-08 15:56:49', 'upstream', migration))


# 12. Shad 05FJ: upstream migration should start from 25/03/2025 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "05FJ" & arrival > '2025-03-25 21:15:52' & arrival < '2025-03-28 03:37:04', 'upstream', migration))


# 13. Shad 22084666: downstream migration should start from 10/05/2023 till 22/05/2023 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084666" & arrival > '2023-05-10 06:20:43' & arrival < '2023-05-22 11:24:00', 'downstream', migration))
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084666" & arrival > '2023-05-23 16:16:20' & arrival < '2023-07-02 17:22:48', NA, migration))


# 14. Shad 1308527: upstream migration should start from 16/04/2021 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1308527" & arrival < '2021-04-16 00:51:20' & arrival > '2021-03-25 20:55:00', NA, migration))


# 15. Shad 05FY: downstream migration should end at 05/05/2025 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "05FY" & arrival > '2025-05-05 09:00:00', NA, migration))


# 16. Shad 05GZ: upstream migration should start from 15/04/2025 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "05GZ" & arrival < '2025-04-15 22:06:17' & arrival > '2025-04-10 00:00:00', NA, migration))


# 17. Shad 05GZ: downstream migration should end at 15/05/2025 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "05GZ" & arrival > '2025-05-14 20:12:25' & arrival < '2025-05-16 02:33:05', 'downstream', migration))


# 18. Shad 05HH: upstream migration should start from 10/04/2025 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "05HH" & arrival < '2025-04-10 20:23:38' & arrival > '2025-04-07 17:49:10', NA, migration))


# 19. Shad 1308530: upstream migration should start from 01/05/2021 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1308530" & arrival < '2021-05-01 00:11:37' & arrival > '2021-04-20 10:50:19', NA, migration))


# 20. Shad 1308536: upstream migration should start from 12/04/2020 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1308536" & arrival < '2020-04-12 00:24:58' & arrival > '2020-04-04 08:57:08', NA, migration))


# 21. Shad 1367836: upstream migration should start from 09/04/2023 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1367836" & arrival < '2023-04-09 05:08:52' & arrival > '2023-03-25 00:00:00', NA, migration))


# 22. Shad 1367846: upstream migration should start from 27/04/2023 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1367846" & arrival < '2023-04-27 00:23:09' & arrival > '2023-04-26 12:48:34', NA, migration))


# 23. Shad 1367854: upstream migration should start from 04/04/2022 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1367854" & arrival < '2022-04-04 21:40:12' & arrival > '2022-03-24 14:55:06', NA, migration))


# 23. Shad 1367874: upstream migration should start from 05/04/2022 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "1367874" & arrival < '2022-04-05 23:53:43' & arrival > '2022-03-05 00:00:00', NA, migration))


# 24. Shad 22084650: upstream migration should start from 06/04/2023 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084650" & arrival < '2023-04-06 09:23:58' & arrival > '2022-03-28 22:07:57', NA, migration))


# 25. Shad 22084687: upstream migration should start from 11/04/2023 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084687" & arrival < '2023-04-11 15:05:00' & arrival > '2022-03-31 03:41:01', NA, migration))

# 26. Shad 22084672: downstream migration should end at 31/05/2023 ####
data_with_spawning_behavior <- data_with_spawning_behavior %>%
  mutate(migration = ifelse(tag_serial_number == "22084672" & arrival > '2023-05-31 00:00:00', NA, migration))


# Remove redundant column
data_with_spawning_behavior$tag_serial_number_value <- NULL

# Write csv file
write.csv(data_with_spawning_behavior, "./data/interim/data_with_flag_spawning_migration.csv")



# Code to test above settings

#subset <- filter(data_with_spawning_behavior, tag_serial_number == "23087277",
#                 arrival > '2024-04-03 17:29:58',
#                 arrival < '2024-05-25 00:00:00')

#subset <- select(subset, tag_serial_number, arrival, station_name, migration)

#subset2 <- subset %>%
#  mutate(migration = ifelse(tag_serial_number == "23087277" & arrival > '2024-04-10 00:36:23' & arrival < '2024-04-12 19:01:40', 'spawning', migration))

#View(subset)
#View(subset2)
