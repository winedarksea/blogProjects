# data prep
library(tidyr)
library(dplyr)

# Preparing CSV to RDA

setwd("C:/Users/Owner/Downloads/Kelly Training Data/Workout Files/AllFIT")
garmin.data <- read.csv("KellyGarminData.csv")
# cut down some of the excess columns that don't contain information I want.
garmin.data <- garmin.data[,1:51]
# create a unique ID for each garmin ride (separate .fit file before merge)
garmin.data$id <- ifelse(garmin.data$Message == "file_id" & garmin.data$Type == "Data",
                         paste0(garmin.data$X.1, as.character(garmin.data$Value.2)), NA)
# fill the ride ID to all records of that ride
garmin.data <- fill(garmin.data, id, .direction = "down") #also used 'up' for first 5
garmin.data <- fill(garmin.data, id, .direction = "up")

# find any field which is marked as 'power' and record
garmin.data$power_watts <- ifelse(garmin.data$Field.8 == "power", as.numeric(as.character(garmin.data$Value.8)),
                            ifelse(garmin.data$Field.7 == "power", as.numeric(as.character(garmin.data$Value.7)),
                                   ifelse(garmin.data$Field.6 == "power", as.numeric(as.character(garmin.data$Value.6)),
                                          ifelse(garmin.data$Field.5 == "power", as.numeric(as.character(garmin.data$Value.5)),
                                                 ifelse(garmin.data$Field.4 == "power", as.numeric(as.character(garmin.data$Value.4)),
                                                        ifelse(garmin.data$Field.3 == "power", as.numeric(as.character(garmin.data$Value.3)),
                                                               ifelse(garmin.data$Field.2 == "power", as.numeric(as.character(garmin.data$Value.2)),
                                                                      ifelse(garmin.data$Field.12 == "power", as.numeric(as.character(garmin.data$Value.12)),
                                                                             ifelse(garmin.data$Field.13 == "power", as.numeric(as.character(garmin.data$Value.13)),
                                                                                    ifelse(garmin.data$Field.14 == "power", as.numeric(as.character(garmin.data$Value.14)),
                                                                                           ifelse(garmin.data$Field.9 == "power", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                  ifelse(garmin.data$Field.10 == "power", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                         ifelse(garmin.data$Field.11 == "power", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))
# remove fluke max power errors, 1775 was chosen as what Kelly thought her actual max power was
garmin.data$power_watts <- ifelse(garmin.data$power_watts > 1775, NA, garmin.data$power_watts)

garmin.data$heart_rate_bpm <- ifelse(garmin.data$Field.8 == "heart_rate", as.numeric(as.character(garmin.data$Value.8)),
                                 ifelse(garmin.data$Field.7 == "heart_rate", as.numeric(as.character(garmin.data$Value.7)),
                                        ifelse(garmin.data$Field.6 == "heart_rate", as.numeric(as.character(garmin.data$Value.6)),
                                               ifelse(garmin.data$Field.5 == "heart_rate", as.numeric(as.character(garmin.data$Value.5)),
                                                      ifelse(garmin.data$Field.4 == "heart_rate", as.numeric(as.character(garmin.data$Value.4)),
                                                             ifelse(garmin.data$Field.3 == "heart_rate", as.numeric(as.character(garmin.data$Value.3)),
                                                                    ifelse(garmin.data$Field.2 == "heart_rate", as.numeric(as.character(garmin.data$Value.2)),
                                                                           ifelse(garmin.data$Field.12 == "heart_rate", as.numeric(as.character(garmin.data$Value.12)),
                                                                                  ifelse(garmin.data$Field.13 == "heart_rate", as.numeric(as.character(garmin.data$Value.13)),
                                                                                         ifelse(garmin.data$Field.14 == "heart_rate", as.numeric(as.character(garmin.data$Value.14)),
                                                                                                ifelse(garmin.data$Field.9 == "heart_rate", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                       ifelse(garmin.data$Field.10 == "heart_rate", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                              ifelse(garmin.data$Field.11 == "heart_rate", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))

garmin.data$cadence_rpm <- ifelse(garmin.data$Field.8 == "cadence", as.numeric(as.character(garmin.data$Value.8)),
                              ifelse(garmin.data$Field.7 == "cadence", as.numeric(as.character(garmin.data$Value.7)),
                                     ifelse(garmin.data$Field.6 == "cadence", as.numeric(as.character(garmin.data$Value.6)),
                                            ifelse(garmin.data$Field.5 == "cadence", as.numeric(as.character(garmin.data$Value.5)),
                                                   ifelse(garmin.data$Field.4 == "cadence", as.numeric(as.character(garmin.data$Value.4)),
                                                          ifelse(garmin.data$Field.3 == "cadence", as.numeric(as.character(garmin.data$Value.3)),
                                                                 ifelse(garmin.data$Field.2 == "cadence", as.numeric(as.character(garmin.data$Value.2)),
                                                                        ifelse(garmin.data$Field.12 == "cadence", as.numeric(as.character(garmin.data$Value.12)),
                                                                               ifelse(garmin.data$Field.13 == "cadence", as.numeric(as.character(garmin.data$Value.13)),
                                                                                      ifelse(garmin.data$Field.14 == "cadence", as.numeric(as.character(garmin.data$Value.14)),
                                                                                             ifelse(garmin.data$Field.9 == "cadence", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                    ifelse(garmin.data$Field.10 == "cadence", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                           ifelse(garmin.data$Field.11 == "cadence", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))

garmin.data$accumulated_power_watts <- ifelse(garmin.data$Field.8 == "accumulated_power", as.numeric(as.character(garmin.data$Value.8)),
                                        ifelse(garmin.data$Field.7 == "accumulated_power", as.numeric(as.character(garmin.data$Value.7)),
                                               ifelse(garmin.data$Field.6 == "accumulated_power", as.numeric(as.character(garmin.data$Value.6)),
                                                      ifelse(garmin.data$Field.5 == "accumulated_power", as.numeric(as.character(garmin.data$Value.5)),
                                                             ifelse(garmin.data$Field.4 == "accumulated_power", as.numeric(as.character(garmin.data$Value.4)),
                                                                    ifelse(garmin.data$Field.3 == "accumulated_power", as.numeric(as.character(garmin.data$Value.3)),
                                                                           ifelse(garmin.data$Field.2 == "accumulated_power", as.numeric(as.character(garmin.data$Value.2)),
                                                                                  ifelse(garmin.data$Field.12 == "accumulated_power", as.numeric(as.character(garmin.data$Value.12)),
                                                                                         ifelse(garmin.data$Field.13 == "accumulated_power", as.numeric(as.character(garmin.data$Value.13)),
                                                                                                ifelse(garmin.data$Field.14 == "accumulated_power", as.numeric(as.character(garmin.data$Value.14)),
                                                                                                       ifelse(garmin.data$Field.9 == "accumulated_power", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                              ifelse(garmin.data$Field.10 == "accumulated_power", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                                     ifelse(garmin.data$Field.11 == "accumulated_power", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))


garmin.data$distance_m <- ifelse(garmin.data$Field.8 == "distance", as.numeric(as.character(garmin.data$Value.8)),
                               ifelse(garmin.data$Field.7 == "distance", as.numeric(as.character(garmin.data$Value.7)),
                                      ifelse(garmin.data$Field.6 == "distance", as.numeric(as.character(garmin.data$Value.6)),
                                             ifelse(garmin.data$Field.5 == "distance", as.numeric(as.character(garmin.data$Value.5)),
                                                    ifelse(garmin.data$Field.4 == "distance", as.numeric(as.character(garmin.data$Value.4)),
                                                           ifelse(garmin.data$Field.3 == "distance", as.numeric(as.character(garmin.data$Value.3)),
                                                                  ifelse(garmin.data$Field.2 == "distance", as.numeric(as.character(garmin.data$Value.2)),
                                                                         ifelse(garmin.data$Field.12 == "distance", as.numeric(as.character(garmin.data$Value.12)),
                                                                                ifelse(garmin.data$Field.13 == "distance", as.numeric(as.character(garmin.data$Value.13)),
                                                                                       ifelse(garmin.data$Field.14 == "distance", as.numeric(as.character(garmin.data$Value.14)),
                                                                                              ifelse(garmin.data$Field.9 == "distance", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                     ifelse(garmin.data$Field.10 == "distance", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                            ifelse(garmin.data$Field.11 == "distance", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))

garmin.data$altitude_meters <- ifelse(garmin.data$Field.8 == "altitude", as.numeric(as.character(garmin.data$Value.8)),
                               ifelse(garmin.data$Field.7 == "altitude", as.numeric(as.character(garmin.data$Value.7)),
                                      ifelse(garmin.data$Field.6 == "altitude", as.numeric(as.character(garmin.data$Value.6)),
                                             ifelse(garmin.data$Field.5 == "altitude", as.numeric(as.character(garmin.data$Value.5)),
                                                    ifelse(garmin.data$Field.4 == "altitude", as.numeric(as.character(garmin.data$Value.4)),
                                                           ifelse(garmin.data$Field.3 == "altitude", as.numeric(as.character(garmin.data$Value.3)),
                                                                  ifelse(garmin.data$Field.2 == "altitude", as.numeric(as.character(garmin.data$Value.2)),
                                                                         ifelse(garmin.data$Field.12 == "altitude", as.numeric(as.character(garmin.data$Value.12)),
                                                                                ifelse(garmin.data$Field.13 == "altitude", as.numeric(as.character(garmin.data$Value.13)),
                                                                                       ifelse(garmin.data$Field.14 == "altitude", as.numeric(as.character(garmin.data$Value.14)),
                                                                                              ifelse(garmin.data$Field.9 == "altitude", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                     ifelse(garmin.data$Field.10 == "altitude", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                            ifelse(garmin.data$Field.11 == "altitude", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))


garmin.data$temperature_C <- ifelse(garmin.data$Field.8 == "temperature", as.numeric(as.character(garmin.data$Value.8)),
                                  ifelse(garmin.data$Field.7 == "temperature", as.numeric(as.character(garmin.data$Value.7)),
                                         ifelse(garmin.data$Field.6 == "temperature", as.numeric(as.character(garmin.data$Value.6)),
                                                ifelse(garmin.data$Field.5 == "temperature", as.numeric(as.character(garmin.data$Value.5)),
                                                       ifelse(garmin.data$Field.4 == "temperature", as.numeric(as.character(garmin.data$Value.4)),
                                                              ifelse(garmin.data$Field.3 == "temperature", as.numeric(as.character(garmin.data$Value.3)),
                                                                     ifelse(garmin.data$Field.2 == "temperature", as.numeric(as.character(garmin.data$Value.2)),
                                                                            ifelse(garmin.data$Field.12 == "temperature", as.numeric(as.character(garmin.data$Value.12)),
                                                                                   ifelse(garmin.data$Field.13 == "temperature", as.numeric(as.character(garmin.data$Value.13)),
                                                                                          ifelse(garmin.data$Field.14 == "temperature", as.numeric(as.character(garmin.data$Value.14)),
                                                                                                 ifelse(garmin.data$Field.9 == "temperature", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                        ifelse(garmin.data$Field.10 == "temperature", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                               ifelse(garmin.data$Field.11 == "temperature", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))


garmin.data$left_right_balance <- ifelse(garmin.data$Field.8 == "left_right_balance", as.numeric(as.character(garmin.data$Value.8)),
                                         ifelse(garmin.data$Field.7 == "left_right_balance", as.numeric(as.character(garmin.data$Value.7)),
                                                ifelse(garmin.data$Field.6 == "left_right_balance", as.numeric(as.character(garmin.data$Value.6)),
                                                       ifelse(garmin.data$Field.5 == "left_right_balance", as.numeric(as.character(garmin.data$Value.5)),
                                                              ifelse(garmin.data$Field.4 == "left_right_balance", as.numeric(as.character(garmin.data$Value.4)),
                                                                     ifelse(garmin.data$Field.3 == "left_right_balance", as.numeric(as.character(garmin.data$Value.3)),
                                                                            ifelse(garmin.data$Field.2 == "left_right_balance", as.numeric(as.character(garmin.data$Value.2)),
                                                                                   ifelse(garmin.data$Field.12 == "left_right_balance", as.numeric(as.character(garmin.data$Value.12)),
                                                                                          ifelse(garmin.data$Field.13 == "left_right_balance", as.numeric(as.character(garmin.data$Value.13)),
                                                                                                 ifelse(garmin.data$Field.14 == "left_right_balance", as.numeric(as.character(garmin.data$Value.14)),
                                                                                                        ifelse(garmin.data$Field.9 == "left_right_balance", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                               ifelse(garmin.data$Field.10 == "left_right_balance", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                                      ifelse(garmin.data$Field.11 == "left_right_balance", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))




garmin.data$left_torque_effectiveness <- ifelse(garmin.data$Field.8 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.8)),
                                         ifelse(garmin.data$Field.7 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.7)),
                                                ifelse(garmin.data$Field.16 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.16)),
                                                       ifelse(garmin.data$Field.15 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.15)),
                                                              ifelse(garmin.data$Field.4 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.4)),
                                                                     ifelse(garmin.data$Field.3 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.3)),
                                                                            ifelse(garmin.data$Field.2 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.2)),
                                                                                   ifelse(garmin.data$Field.12 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.12)),
                                                                                          ifelse(garmin.data$Field.13 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.13)),
                                                                                                 ifelse(garmin.data$Field.14 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.14)),
                                                                                                        ifelse(garmin.data$Field.9 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                               ifelse(garmin.data$Field.10 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                                      ifelse(garmin.data$Field.11 == "left_torque_effectiveness", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))






garmin.data$left_pedal_smoothness <- ifelse(garmin.data$Field.8 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.8)),
                                         ifelse(garmin.data$Field.7 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.7)),
                                                ifelse(garmin.data$Field.16 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.16)),
                                                       ifelse(garmin.data$Field.15 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.15)),
                                                              ifelse(garmin.data$Field.4 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.4)),
                                                                     ifelse(garmin.data$Field.3 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.3)),
                                                                            ifelse(garmin.data$Field.2 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.2)),
                                                                                   ifelse(garmin.data$Field.12 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.12)),
                                                                                          ifelse(garmin.data$Field.13 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.13)),
                                                                                                 ifelse(garmin.data$Field.14 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.14)),
                                                                                                        ifelse(garmin.data$Field.9 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                               ifelse(garmin.data$Field.10 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                                      ifelse(garmin.data$Field.11 == "left_pedal_smoothness", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))








garmin.data$speed_m.s <- ifelse(garmin.data$Field.8 == "speed", as.numeric(as.character(garmin.data$Value.8)),
                            ifelse(garmin.data$Field.7 == "speed", as.numeric(as.character(garmin.data$Value.7)),
                                   ifelse(garmin.data$Field.6 == "speed", as.numeric(as.character(garmin.data$Value.6)),
                                          ifelse(garmin.data$Field.5 == "speed", as.numeric(as.character(garmin.data$Value.5)),
                                                 ifelse(garmin.data$Field.4 == "speed", as.numeric(as.character(garmin.data$Value.4)),
                                                        ifelse(garmin.data$Field.3 == "speed", as.numeric(as.character(garmin.data$Value.3)),
                                                               ifelse(garmin.data$Field.2 == "speed", as.numeric(as.character(garmin.data$Value.2)),
                                                                      ifelse(garmin.data$Field.12 == "speed", as.numeric(as.character(garmin.data$Value.12)),
                                                                             ifelse(garmin.data$Field.13 == "speed", as.numeric(as.character(garmin.data$Value.13)),
                                                                                    ifelse(garmin.data$Field.14 == "speed", as.numeric(as.character(garmin.data$Value.14)),
                                                                                           ifelse(garmin.data$Field.9 == "speed", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                  ifelse(garmin.data$Field.10 == "speed", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                         ifelse(garmin.data$Field.11 == "speed", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))


# convert from metric to imperial, even though I like metric
garmin.data$speed_mph <- garmin.data$speed_m.s * 2.23694

garmin.data$Latitude <- ifelse(garmin.data$Field.8 == "position_lat", as.numeric(as.character(garmin.data$Value.8)),
                            ifelse(garmin.data$Field.7 == "position_lat", as.numeric(as.character(garmin.data$Value.7)),
                                   ifelse(garmin.data$Field.6 == "position_lat", as.numeric(as.character(garmin.data$Value.6)),
                                          ifelse(garmin.data$Field.5 == "position_lat", as.numeric(as.character(garmin.data$Value.5)),
                                                 ifelse(garmin.data$Field.4 == "position_lat", as.numeric(as.character(garmin.data$Value.4)),
                                                        ifelse(garmin.data$Field.3 == "position_lat", as.numeric(as.character(garmin.data$Value.3)),
                                                               ifelse(garmin.data$Field.2 == "position_lat", as.numeric(as.character(garmin.data$Value.2)),
                                                                      ifelse(garmin.data$Field.12 == "position_lat", as.numeric(as.character(garmin.data$Value.12)),
                                                                             ifelse(garmin.data$Field.13 == "position_lat", as.numeric(as.character(garmin.data$Value.13)),
                                                                                    ifelse(garmin.data$Field.14 == "position_lat", as.numeric(as.character(garmin.data$Value.14)),
                                                                                           ifelse(garmin.data$Field.9 == "position_lat", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                  ifelse(garmin.data$Field.10 == "position_lat", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                         ifelse(garmin.data$Field.11 == "position_lat", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))


garmin.data$Longitude <- ifelse(garmin.data$Field.8 == "position_long", as.numeric(as.character(garmin.data$Value.8)),
                            ifelse(garmin.data$Field.7 == "position_long", as.numeric(as.character(garmin.data$Value.7)),
                                   ifelse(garmin.data$Field.6 == "position_long", as.numeric(as.character(garmin.data$Value.6)),
                                          ifelse(garmin.data$Field.5 == "position_long", as.numeric(as.character(garmin.data$Value.5)),
                                                 ifelse(garmin.data$Field.4 == "position_long", as.numeric(as.character(garmin.data$Value.4)),
                                                        ifelse(garmin.data$Field.3 == "position_long", as.numeric(as.character(garmin.data$Value.3)),
                                                               ifelse(garmin.data$Field.2 == "position_long", as.numeric(as.character(garmin.data$Value.2)),
                                                                      ifelse(garmin.data$Field.12 == "position_long", as.numeric(as.character(garmin.data$Value.12)),
                                                                             ifelse(garmin.data$Field.13 == "position_long", as.numeric(as.character(garmin.data$Value.13)),
                                                                                    ifelse(garmin.data$Field.14 == "position_long", as.numeric(as.character(garmin.data$Value.14)),
                                                                                           ifelse(garmin.data$Field.9 == "position_long", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                  ifelse(garmin.data$Field.10 == "position_long", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                         ifelse(garmin.data$Field.11 == "position_long", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))




# convert 'semicircles' to Latitude and Longitude
garmin.data$Latitude <- garmin.data$Latitude *(180/2^31)
garmin.data$Longitude <- garmin.data$Longitude *(180/2^31)

garmin.data$timestamp <- ifelse(garmin.data$Field.8 == "timestamp", as.numeric(as.character(garmin.data$Value.8)),
                                ifelse(garmin.data$Field.7 == "timestamp", as.numeric(as.character(garmin.data$Value.7)),
                                       ifelse(garmin.data$Field.6 == "timestamp", as.numeric(as.character(garmin.data$Value.6)),
                                              ifelse(garmin.data$Field.5 == "timestamp", as.numeric(as.character(garmin.data$Value.5)),
                                                     ifelse(garmin.data$Field.4 == "timestamp", as.numeric(as.character(garmin.data$Value.4)),
                                                            ifelse(garmin.data$Field.3 == "timestamp", as.numeric(as.character(garmin.data$Value.3)),
                                                                   ifelse(garmin.data$Field.2 == "timestamp", as.numeric(as.character(garmin.data$Value.2)),
                                                                          ifelse(garmin.data$Field.12 == "timestamp", as.numeric(as.character(garmin.data$Value.12)),
                                                                                 ifelse(garmin.data$Field.13 == "timestamp", as.numeric(as.character(garmin.data$Value.13)),
                                                                                        ifelse(garmin.data$Field.1 == "timestamp", as.numeric(as.character(garmin.data$Value.1)),
                                                                                               ifelse(garmin.data$Field.9 == "timestamp", as.numeric(as.character(garmin.data$Value.9)),
                                                                                                      ifelse(garmin.data$Field.10 == "timestamp", as.numeric(as.character(garmin.data$Value.10)),
                                                                                                             ifelse(garmin.data$Field.11 == "timestamp", as.numeric(as.character(garmin.data$Value.11)), NA)))))))))))))





##### Convert timestamps into more usable values, I've got some experimental versions commented out
# garmin.data$Value.2 <- as.Date(as.POSIXct(as.character(garmin.data$Value.1), origin="1989-12-31", tz = "UTC"), "UTC", "%Y-%m-%d")
## First remove erroroneous data
garmin.data$timestamp <- ifelse(garmin.data$timestamp < 1000, NA, garmin.data$timestamp)
garmin.data$timestamp <- ifelse(garmin.data$timestamp >= 948653299, NA, garmin.data$timestamp)
## Convert out of POSIX
garmin.data$Date_GMT <- as.Date(format(as.POSIXct(garmin.data$timestamp,origin = "1989-12-31",tz = "GMT"),format="%Y-%m-%d"),format="%Y-%m-%d")
# format(as.POSIXct(850345613,origin = "1989-12-31",tz = "GMT"),format="%Y-%m-%d-%H-%M-%S")
# as.DateTime(format(as.POSIXct(850345613,origin = "1989-12-31",tz = "GMT"),format="%Y-%m-%d-%H-%M-%S"),format="%Y-%m-%d-%H-%M-%S")

# Give a row number, per ride
garmin.data <- garmin.data %>% group_by(id) %>% mutate(ride_time = row_number()) %>% ungroup()


# saveRDS(garmin.data, "GarminData1.2.rda") #6129336 obs of 69 variables
# garmin.data <- readRDS("GarminData.rda")

#### selecting a smaller version of the data with only 'cleaned' columns:
# garmin.data <- garmin.data %>% select(id,timestamp, Date_GMT, ride_time, Latitude, Longitude, 
#                                       power_watts, heart_rate_bpm, cadence_rpm,
#                                       accumulated_power_watts, distance_m,altitude_meters, speed_m.s, speed_mph,
#                                       temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
#                                       five_second_average, 
#                                       ten_second_average, thirty_second_average, one_minute_average, three_minute_average, 
#                                       five_minute_average, ten_minute_average, thirty_minute_average, one_hour_average
#                                       )
# 5932430 obs of 27 variables
# saveRDS(garmin.data[garmin.data$Type == "Data" & garmin.data$Message == "record",52:68], "GarminDataClean.rda") # saveRDS(garmin.data,"GarminDataClean.rda")
