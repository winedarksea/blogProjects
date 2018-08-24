# power analysis
library(dplyr)
library(lubridate)
library(ggplot2)
setwd("C:/Users/Owner/Downloads/Kelly Training Data/Workout Files/AllFIT")
garmin.data <- readRDS("GarminDataClean1.2.rda") #5932430 obs of 54 variables

# for plotting multiple graphs, borrowed from the internet, I've lost track of where from...
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


################ Creating bins of rolling averages across all of the data
library(zoo)
# library(doParallel)
# cl <- makeCluster(4)
# registerDoParallel(cl)

n = 10
name = "ten_second_average"
garmin.data[, name] <- rollapply(garmin.data$power_watts, width = n, mean, fill = NA, align = "right")
garmin.data <- garmin.data %>% mutate(lag = lag(id, (n-1)))
garmin.data[, name] <- ifelse(garmin.data$lag != garmin.data$id, NA, garmin.data[, name])
garmin.data$lag <- NULL

intervals <- c(5, 10, 30, 60, 180, 300, 600, 1800, 3600)
interval_names <- c("five_second_average","ten_second_average", "thirty_second_average",
                    "one_minute_average","three_minute_average","five_minute_average",
                    "ten_minute_average", "thirty_minute_average", "one_hour_average")
length(intervals) == length(interval_names)
for (i in 1:length(intervals)) {
  n = intervals[i]
  name = interval_names[i]
  garmin.data[, name] <- rollapply(garmin.data$power_watts, width = n, mean, fill = NA, align = "right")
  garmin.data <- garmin.data %>% mutate(lag = lag(id, (n-1)))
  garmin.data[, name] <- ifelse(garmin.data$lag != garmin.data$id, NA, garmin.data[, name])
  garmin.data$lag <- NULL
}

# Same thing for cadence
intervals <- c(5, 10, 30, 60, 180, 300, 600, 1800, 3600)
interval_names <- c("five_second_cadence_average","ten_second_cadence_average", "thirty_second_cadence_average",
                    "one_minute_cadence_average","three_minute_cadence_average","five_minute_cadence_average",
                    "ten_minute_cadence_average", "thirty_minute_cadence_average", "one_hour_cadence_average")
length(intervals) == length(interval_names)
for (i in 1:length(intervals)) {
  n = intervals[i]
  name = interval_names[i]
  garmin.data[, name] <- rollapply(garmin.data$cadence_rpm, width = n, mean, fill = NA, align = "right")
  garmin.data <- garmin.data %>% mutate(lag = lag(id, (n-1)))
  garmin.data[, "the.test"] <- ifelse(garmin.data$lag != garmin.data$id, NA, 1)
  garmin.data[is.na(garmin.data$the.test), name] <- NA
  garmin.data$the.test <- NULL
  garmin.data$lag <- NULL
}

# Same thing for heart rate

intervals <- c(5, 10, 30, 60, 180, 300, 600, 1800, 3600)
interval_names <- c("five_second_heartrate_average","ten_second_heartrate_average", "thirty_second_heartrate_average",
                    "one_minute_heartrate_average","three_minute_heartrate_average","five_minute_heartrate_average",
                    "ten_minute_heartrate_average", "thirty_minute_heartrate_average", "one_hour_heartrate_average")
length(intervals) == length(interval_names)
for (i in 1:length(intervals)) {
  n = intervals[i]
  name = interval_names[i]
  garmin.data[, name] <- rollapply(garmin.data$heart_rate_bpm, width = n, mean, fill = NA, align = "right")
  garmin.data <- garmin.data %>% mutate(lag = lag(id, (n-1)))
  garmin.data[, "the.test"] <- ifelse(garmin.data$lag != garmin.data$id, NA, 1)
  garmin.data[is.na(garmin.data$the.test), name] <- NA
  garmin.data$the.test <- NULL
  garmin.data$lag <- NULL
}

# For speed averages
intervals <- c(5, 10, 30, 60, 180, 300, 600, 1800, 3600)
interval_names <- c("five_second_speed_average","ten_second_speed_average", "thirty_second_speed_average",
                    "one_minute_speed_average","three_minute_speed_average","five_minute_speed_average",
                    "ten_minute_speed_average", "thirty_minute_speed_average", "one_hour_speed_average")
length(intervals) == length(interval_names)
for (i in 1:length(intervals)) {
  n = intervals[i]
  name = interval_names[i]
  garmin.data[, name] <- rollapply(garmin.data$speed_mph, width = n, mean, fill = NA, align = "right")
  garmin.data <- garmin.data %>% mutate(lag = lag(id, (n-1)))
  garmin.data[, "the.test"] <- ifelse(garmin.data$lag != garmin.data$id, NA, 1)
  garmin.data[is.na(garmin.data$the.test), name] <- NA
  garmin.data$the.test <- NULL
  garmin.data$lag <- NULL
}

# colMax <- do.call(pmax, garmin.data)
# colMax <- data.frame(group = rep(c("power", "cadence", "heart_rate", "speed"), each =  9),
#                      average = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 4),
#                      averages = do.call(pmax, garmin.data[,18:54]))

# check that the na's are present for the first row
# temp <- garmin.data %>% group_by(id) %>% slice(1) %>% head()

# saveRDS(garmin.data, "GarminDataClean1.2.rda")

# Power Curves
date <- "2018-01-01"
date2 <- "2018-05-30"
date3 <- "2017-01-01"
date4 <- "2017-05-30"
# power
curve1 <- garmin.data %>% filter(Date_GMT >= date, Date_GMT <= date2) %>% select(19:27)
curve1 <- apply(curve1, 2, max, na.rm = T)
curve2 <- garmin.data %>% filter(Date_GMT >= date3, Date_GMT <= date4) %>% select(19:27)
curve2 <- apply(curve2, 2, max, na.rm = T)
curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                     group = rep(c(paste(year(date), "Selection 1"),paste(year(date3), "Selection 2")), each = 9),
                     max = append(as.vector(curve1), as.vector(curve2)))
curves <- curves %>% mutate(log.duration = log(duration))
g1 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
  ylab("Max Average Power Watts") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour") +
  theme_bw() + theme(legend.position="bottom") + 
  geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
# Speed
curve1 <- garmin.data %>% filter(Date_GMT >= date, Date_GMT <= date2) %>% select(46:54)
curve1 <- apply(curve1, 2, max, na.rm = T)
curve2 <- garmin.data %>% filter(Date_GMT >= date3, Date_GMT <= date4) %>% select(46:54)
curve2 <- apply(curve2, 2, max, na.rm = T)
curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                     group = rep(c(paste(year(date), "Selection 1"),paste(year(date3), "Selection 2")), each = 9),
                     max = append(as.vector(curve1), as.vector(curve2)))
curves <- curves %>% mutate(log.duration = log(duration))
g2 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
  ylab("Max Average Speed MPH") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour")+
  theme_bw() + theme(legend.position="bottom")+
  geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
# cadence
curve1 <- garmin.data %>% filter(Date_GMT >= date, Date_GMT <= date2) %>% select(28:36)
curve1 <- apply(curve1, 2, max, na.rm = T)
curve2 <- garmin.data %>% filter(Date_GMT >= date3, Date_GMT <= date4) %>% select(28:36)
curve2 <- apply(curve2, 2, max, na.rm = T)
curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                     group = rep(c(paste(year(date), "Selection 1"),paste(year(date3), "Selection 2")), each = 9),
                     max = append(as.vector(curve1), as.vector(curve2)))
curves <- curves %>% mutate(log.duration = log(duration))
g3 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
  ylab("Max Average Cadence RPM") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour")+
  theme_bw() + theme(legend.position="bottom")+
  geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
# heart rate
curve1 <- garmin.data %>% filter(Date_GMT >= date, Date_GMT <= date2) %>% select(37:45)
curve1 <- apply(curve1, 2, max, na.rm = T)
curve2 <- garmin.data %>% filter(Date_GMT >= date3, Date_GMT <= date4) %>% select(37:45)
curve2 <- apply(curve2, 2, max, na.rm = T)
curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                     group = rep(c(paste(year(date), "Selection 1"),paste(year(date3), "Selection 2")), each = 9),
                     max = append(as.vector(curve1), as.vector(curve2)))
curves <- curves %>% mutate(log.duration = log(duration))
g4 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
  ylab("Max Average Heart Rate") + xlab("Log of Duration, Labels in Seconds") +
  theme_bw() + theme(legend.position="bottom") + 
  geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
multiplot(g1, g2, g3, g4, cols=2)


g1 <- ggplot(curves[curves$duration <=300,], aes(x = duration, y = max, color = group)) + geom_line() + geom_point() +
  ylab("Max Heart Rate") + xlab("Duration in Seconds")+ theme(legend.position="bottom")
g2 <- ggplot(curves[curves$duration >= 60,], aes(x = duration, y = max, color = group)) + geom_line() + geom_point() +
  ylab("Max Heart Rate") + xlab("Log of Duration, 5 sec to 1 hour") + theme(legend.position="bottom")
multiplot(g1, g2, cols=2)

####################################################

### Top N versus Top N lower
name <- "five_minute"
n <- 100
column = "five_minute_average"
column = "temperature_C"
garmin.data[which.max(garmin.data[,column]),c(1,4,8:18)]
temp <- garmin.data %>% top_n(3, wt = five_minute_speed_average)
garmin.data[which.max(garmin.data$five_second_average),c(1,4,8:18)]
# work

# grep("five_minute")
column <- paste0(name, "_average")
top_5 <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
  group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup() %>%
  arrange_(column) %>% tail(5) %>%
  select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
  ) %>%
  summarise_all(mean, na.rm = T)
top_10 <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
  group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup() %>%
  arrange_(column) %>% tail(10) %>%
  select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
  ) %>%
  summarise_all(mean, na.rm = T)
top_10.index <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
  group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup() %>%
  arrange_(column) %>% tail(10) %>% select(id)
top_100 <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601, !(id %in% top_10.index$id)) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
  group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup() %>%
  arrange_(column) %>% tail(10) %>%
  select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
  ) %>%
  summarise_all(mean, na.rm = T)
top_1000 <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
  group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup() %>%
  arrange_(column) %>% tail(1000) %>%
  select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
  ) %>%
  summarise_all(mean, na.rm = T)
top_comparison <- rbind(top_5,top_10, top_100, top_1000)
row.names(top_comparison) <- c("Top 5","Top 10", "Next Top 100", "All Top 1000")
top_comparison



######## Not changeable way
# find top 5
top_5 <- garmin.data %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/100),0)*100))) %>%
  group_by(id, rounded) %>% top_n(1, wt = five_minute_average)%>% ungroup() %>% 
  top_n(5, wt = five_minute_average)%>%
  select(five_minute_average, ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         five_minute_cadence_average, five_minute_heartrate_average, five_minute_speed_average
         ) %>%
  summarise_all(mean, na.rm = T)
top_10 <- garmin.data %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/100),0)*100))) %>%
  group_by(id, rounded) %>% top_n(1, wt = five_minute_average)%>% ungroup() %>% 
  top_n(10, wt = five_minute_average) %>%
  select(five_minute_average, ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         five_minute_cadence_average, five_minute_heartrate_average, five_minute_speed_average
  ) %>%
  summarise_all(mean, na.rm = T)
#indexes of top 5
top_10.index <- garmin.data %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/100),0)*100))) %>%
  group_by(id, rounded) %>% top_n(1, wt = five_minute_average)%>% ungroup() %>% 
  top_n(10, wt = five_minute_average) %>% select(id)
# top 100 less top 5
top_100 <- garmin.data %>% filter(id != 321208820778601, !(id %in% top_10.index$id)) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/100),0)*100))) %>%
  group_by(id, rounded) %>% top_n(1, wt = five_minute_average)%>% ungroup() %>% 
  top_n(100, wt = five_minute_average)%>%
  select(five_minute_average, ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         five_minute_cadence_average, five_minute_heartrate_average, five_minute_speed_average
  ) %>%
  summarise_all(mean, na.rm = T)
top_1000 <- garmin.data %>% filter(id != 321208820778601) %>%
  mutate(rounded = as.factor(as.integer(round((ride_time/100),0)*100))) %>%
  group_by(id, rounded) %>% top_n(1, wt = five_minute_average)%>% ungroup() %>% 
  top_n(100, wt = five_minute_average)%>%
  select(five_minute_average, ride_time, accumulated_power_watts, distance_m, altitude_meters,
         temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
         five_minute_cadence_average, five_minute_heartrate_average, five_minute_speed_average
  ) %>%
  summarise_all(mean, na.rm = T)

top_comparison <- rbind(top_5,top_10, top_100, top_1000)
row.names(top_comparison) <- c("Top 5","Top 10", "Next Top 100", "All Top 1000")

filter(garmin.data[,names(garmin.data)[1]] %in% column)%>% top_n(5, wt = five_minute_speed_average)
dplyr::filter(df, as.data.frame(df)[,names(df)[1]] %in% vals)

library(lazyeval)
df <- dplyr::tbl_df(df)
expr <- lazyeval::interp(quote(x %in% y), x = as.name(names(df)[1]), y = vals)

df %>% filter_(expr)

### Linear Regression
linReg <- garmin.data %>%
  select(-c(Latitude, Longitude, heart_rate_bpm,five_second_heartrate_average, 
            ten_second_heartrate_average, thirty_second_heartrate_average, 
            one_minute_heartrate_average, three_minute_heartrate_average, five_minute_heartrate_average, 
            ten_minute_heartrate_average, thirty_minute_heartrate_average, one_hour_heartrate_average, 
            left_right_balance, left_torque_effectiveness, left_pedal_smoothness,five_second_speed_average, 
            ten_second_speed_average, thirty_second_speed_average, one_minute_speed_average, 
            three_minute_speed_average, five_minute_speed_average, ten_minute_speed_average,
            thirty_minute_speed_average, one_hour_speed_average,
            one_hour_average,thirty_minute_average,thirty_minute_cadence_average, one_hour_cadence_average,
            speed_m.s)) %>%
  mutate(speed_mph = ifelse(is.na(speed_mph), 0, speed_mph)) %>%
  na.omit %>%
  group_by(id) %>% top_n(10, wt = five_minute_average) %>% 
  ungroup() %>% top_n(1000, wt = five_minute_average)

fit <- lm(x ~ ride_time+ Latitude+ Longitude+ power_watts+ heart_rate_bpm+ 
            cadence_rpm+ accumulated_power_watts+ distance_m+ altitude_meters+ speed_m.s+ speed_mph+ 
            temperature_C+ left_right_balance+ left_torque_effectiveness+ left_pedal_smoothness + 
            five_second_average+ ten_second_average+ thirty_second_average+ one_minute_average+ 
            three_minute_average+ five_minute_average+
            ten_minute_average+ thirty_minute_average+ one_hour_average+ five_second_cadence_average + 
            ten_second_cadence_average+ thirty_second_cadence_average+ one_minute_cadence_average+ 
            three_minute_cadence_average+ five_minute_cadence_average+ ten_minute_cadence_average+ 
            thirty_minute_cadence_average+ one_hour_cadence_average+ five_second_heartrate_average+ 
            ten_second_heartrate_average+ thirty_second_heartrate_average+ one_minute_heartrate_average+ 
            three_minute_heartrate_average+ five_minute_heartrate_average+ ten_minute_heartrate_average+ 
            thirty_minute_heartrate_average+ one_hour_heartrate_average+ five_second_speed_average+ 
            ten_second_speed_average+ thirty_second_speed_average+ one_minute_speed_average+ 
            three_minute_speed_average+ five_minute_speed_average+ ten_minute_speed_average+ 
            thirty_minute_speed_average+ one_hour_speed_average, data = garmin.data)
fit <- lm(five_minute_average ~ ., data = linReg[,-c(1:3, 5, 6, 12:16, 19:23)])
library(stargazer)
stargazer(fit)
summary(fit)
### Random Forest Variable Importance
library(randomForest)
# library(caret)
rfReg <- linReg[,-c(1:3, 12:16)]
rfFit <- randomForest(five_minute_average ~ ., data=rfReg)
varImportance <- importance(rfFit)
varImportance <- data.frame(x = row.names(varImportance), y = varImportance[,1])
ggplot(varImportance, aes(x = x, y = y)) + geom_bar(stat = 'identity')+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
