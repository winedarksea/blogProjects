# Team Infinite Loop
#  Claire, Colin, Trang

library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinythemes)
library(lubridate)
library(randomForest)
library(leaflet)
library(stargazer)

# from Stackoverflow, also available in library(Rmisc)
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

############## LOAD DATA

# setwd("C:/Users/Owner/Downloads/Kelly Training Data/Workout Files/AllFIT")

setwd("C:/Users/Owner/Documents/School/Spring 2018/PUBH R Class/ShinyProject/Team_Infinite_Loop")
leaflet.data <- readRDS("leaflet.data.rda")
Workout.Summaries <- read.csv("WorkoutSummariesMarch15toMarch18.csv")
names(Workout.Summaries)[c(13,15,24,25)] <- c("Calories", "Hours.Training","Intensity.Factor","Training.Stress.Score")
Workout.Summaries$WorkoutDay <- as.Date(Workout.Summaries$WorkoutDay)
Workout.Summaries$Average.Speed.MPH <- Workout.Summaries$VelocityAverage * 2.23694
setwd("C:/Users/Owner/Downloads/Kelly Training Data/Workout Files/AllFIT")
# this is a really big file, so it won't be on Github, also best to preload before app, then comment out
# garmin.data <- readRDS("GarminDataClean1.2.rda")


###############################
# Create app
ui <- fluidPage(
tabsetPanel(
  tabPanel("Summary", 
  theme = shinytheme("united"),  
  tags$head(tags$style(
    HTML('
         #mapMain {
         background-color: #000000;
         }
         #map2 {
         background-color: #000000;
         }
         #mapTitle {
         text-align: center;
         }')
  )),
  # shinythemes::themeSelector(),
   # Application title
   titlePanel("Kelly Catlin Cycling Data",windowTitle = "Summaries by Day"),
   
   # Sidebar with a slider input for number of bins 
       # sidebarLayout(
      fluidRow(column(width = 3,
        
          dateInput("StartDate", "Start Date:", format = "mm/dd/yy", value = "2016-01-01",
                    min = "2015-03-01", max = "2018-03-31"),
          dateInput("EndDate", "End Date:", format = "mm/dd/yy", value = "2017-01-01",
                    min = "2015-03-01", max = "2018-03-31"),
          selectInput("Stat", label = "Choose stat to display:", choices = c(
            "Total Training Time" = "Hours.Training",
            "Calories Burned" = "Calories",
            "Total Distance in Meters" = "DistanceInMeters",
            "Daily Average Power" = "PowerAverage",
            "Intensity Factor" = "Intensity.Factor",
            "Training Stress Score" = "Training.Stress.Score",
            "Heart Rate Average" = "HeartRateAverage",
            "Average Cadence" = "CadenceAverage",
            "Max Power" = "PowerMax",
            "Average Speed" =  "Average.Speed.MPH"
          )),
          checkboxInput("cyclingOnly", "Show Only Cycling", FALSE),
          actionButton(inputId = 'action', label = "Update Plot")
      ),
      column(width = 9,
      # Show a plot of the generated distribution
         plotOutput("distPlot"),
         textOutput("magic")
      ),
      fluidRow(column(width = 12, img(src='_DSC0473.jpg' , align = "left"))
               
      ),
      fluidRow(column(width = 12, textOutput("caption2"), align = "left"))
   )
  ),
  tabPanel("Ride Start Locations", 
           titlePanel("Unique Ride Start Locations"),
           fluidRow(id="mapMain", column(width = 12, div(style = "height:150px;background-color: black;"),
              leafletOutput("startlocations"))),
           fluidRow(id = "map2",column(width = 12,div(style = "height:300px;background-color: black;")))
           ),
  tabPanel("Power Curve", 
           titlePanel("Maximum Sustained Efforts Across Durations"),
           fluidRow(column(width = 3,
                           # selectInput("average_metric", label = "Choose stat to display:", choices = c(
                           #   "Power" = 1, "Speed" = 4)), #, "Cadence" =3, "Heart Rate" =2
                           dateInput("date", "Start Date Selection 1:", format = "mm/dd/yy", value = "2017-01-01",
                                     min = "2015-03-01", max = "2018-03-31")),
                    column(width = 3,
                           dateInput("date2", "End Date Selection 1:", format = "mm/dd/yy", value = "2017-04-01",
                                     min = "2015-03-01", max = "2018-03-31")),
                    column(width = 3,
                           dateInput("date3", "Start Date Selection 2:", format = "mm/dd/yy", value = "2018-01-01",
                                     min = "2015-03-01", max = "2018-03-31")),
                    column(width = 3,
                           dateInput("date4", "End Date Selection 2:", format = "mm/dd/yy", value = "2018-03-31",
                                     min = "2015-03-01", max = "2018-03-31"))
                           ),
           fluidRow(column(width = 12,
                  # Show a plot of the generated distribution
                  plotOutput("averagePlot", height = "600px"),
                  textOutput("magic2")
           )),
           fluidRow(column(width = 12, img(src='Catlin_Qualifying_Round_2016_Olympics.jpg', align = "left"))),
           fluidRow(column(width = 12, textOutput("caption"), align = "left"))
                    
  ),
  tabPanel("Power Analysis", 
           titlePanel("Comparing Factors of Hardest Efforts"),
           mainPanel(selectInput("name", "Select Time Interval", 
                                 choices = c("five_second", "ten_second", "thirty_second",
                                             "one_minute", "three_minute", "five_minute",
                                             "ten_minute", "thirty_minute", "one_hour"),
                                 selected = "five_minute"),
                     tableOutput("powerAnalysis")
                     
                     
           ),
           fluidRow(column(width = 7,
                           plotOutput("varImport", height = "600px")),
                    column(width = 5,
                           uiOutput("stargazer.html")))
           ),
  tabPanel("View Day Summary",
           mainPanel(
             dateInput("Date", "Selected Date:", format = "mm/dd/yy", value = "2016-08-13",
                       min = "2015-03-01", max = "2018-03-31"),
             tableOutput("table")
             
           ))
  
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  rv <- reactiveValues(p = NULL)
  
  observeEvent(input$action, {
    Workout.Day <- Workout.Summaries %>%
      filter(WorkoutDay >= input$StartDate,
             WorkoutDay < input$EndDate,
             !is.na(Hours.Training),
             WorkoutType != "Day Off")  %>%
      group_by(WorkoutDay, WorkoutType) %>%
      summarise(
        Hours.Training = sum(Hours.Training, na.rm = T),
        Calories = sum(Calories, na.rm = T),
        DistanceInMeters = sum(DistanceInMeters, na.rm = T),
        PowerAverage = (sum(Hours.Training * PowerAverage, na.rm = T)/sum(Hours.Training, na.rm = T)),
        Training.Stress.Score = sum(Training.Stress.Score, na.rm = T),
        Intensity.Factor = sum(Intensity.Factor, na.rm = T),
        HeartRateAverage = median(HeartRateAverage, na.rm = T),
        CadenceAverage= median(CadenceAverage, na.rm = T),
        PowerMax = max(PowerMax, na.rm = T),
        Average.Speed.MPH = median(Average.Speed.MPH, na.rm = T)
      ) %>% filter(Intensity.Factor < 4)
    if (input$cyclingOnly == TRUE) {
      Workout.Day <- Workout.Day %>% filter(WorkoutType == "Bike")
    }
    rv$p <- ggplot(Workout.Day, aes_string(x = Workout.Day$WorkoutDay, y = input$Stat, color = Workout.Day$WorkoutType)) + 
      geom_line() + geom_point() + theme_few() + scale_colour_few() + xlab("")+ 
      guides(color=guide_legend(title = "Workout Type"))
    
  }, ignoreNULL = FALSE)
  output$distPlot <- renderPlot({rv$p})

   output$magic <- renderText ({ #diagnostic tool
     # input$Stat
     format(Sys.time(), format = "%x    %X")
   })
   output$magic2 <- renderText ({ #diagnostic tool
     input$average_metric
     #format(Sys.time(), format = "%x    %X")
   })
   output$startlocations <- renderLeaflet({
       map1 <- leaflet(leaflet.data) %>% addProviderTiles(providers$CartoDB.DarkMatter)  %>% addMarkers( #, size = 15, units = "px"
         clusterOptions = markerClusterOptions()
       ) # intensity =~n
       map1
   })
   output$table <- renderTable({
     Workout.Day <- Workout.Summaries %>%
       filter(WorkoutDay >= input$Date,
              !is.na(Hours.Training),
              WorkoutType != "Day Off")
     subset(Workout.Summaries, WorkoutDay == input$Date)[, c(2:4, 9:16, 18, 20, 21, 24, 25)]
     
   })
   output$powerAnalysis <- renderTable({
     name <- input$name
     column <- paste0(name, "_average")
     n <- 100
     loaded_data <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601)%>%
       mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
       group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup()
     top_5 <- loaded_data %>%
       arrange_(column) %>% tail(5) %>%
       select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
              temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
              paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
       ) %>%
       summarise_all(mean, na.rm = T)
     top_10 <- loaded_data %>%
       arrange_(column) %>% tail(10) %>%
       select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
              temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
              paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
       ) %>%
       summarise_all(mean, na.rm = T)
     top_10.index <- loaded_data %>%
       arrange_(column) %>% tail(10) %>% select(id)
     top_100 <- garmin.data[!is.na(garmin.data[,column]),] %>% 
       filter(id != 321208820778601, !(id %in% top_10.index$id)) %>%
       mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
       group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup() %>%
       arrange_(column) %>% tail(10) %>%
       select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
              temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
              paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
       ) %>%
       summarise_all(mean, na.rm = T)
     top_1000 <- loaded_data %>%
       arrange_(column) %>% tail(1000) %>%
       select(paste0(name, "_average"), ride_time, accumulated_power_watts, distance_m, altitude_meters,
              temperature_C, left_right_balance, left_torque_effectiveness, left_pedal_smoothness,
              paste0(name,"_cadence_average"), paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
       ) %>%
       summarise_all(mean, na.rm = T)
     top_comparison <- rbind(top_5,top_10, top_100, top_1000)
     row.names(top_comparison) <- c("Top 5","Top 10", "Next Top 100", "All Top 1000")
     top_comparison
   }, rownames = TRUE)
   output$caption <- renderText({
     "Kelly, front, pulls during the Qualifying Round of the Rio Olympics"
   })
   output$caption2 <- renderText({
     "Kelly warms up on the local velodrome in Blaine, MN"
   })
   output$stargazer.html <- renderUI({
     name <- input$name
     column <- paste0(name, "_average")
     n <- 100
     loaded_data <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601)%>%
       mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
       group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup()
     linReg <- loaded_data %>%
       select(ride_time, accumulated_power_watts, distance_m, altitude_meters, temperature_C,
              paste0(name, "_average"), paste0(name,"_cadence_average")
               , paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
              )  %>%
       na.omit %>%
       arrange_(column) %>% tail(1000)
     fit <- lm(paste0(column, " ~ ."), data = linReg) 
     # summary(fit)
     HTML(stargazer(fit, type = "html", title = "Linear Regression, Power Inteval as Dependent Var"))
   })
   output$varImport <- renderPlot({
     name <- input$name
     column <- paste0(name, "_average")
     n <- 100
     loaded_data <- garmin.data[!is.na(garmin.data[,column]),] %>% filter(id != 321208820778601)%>%
       mutate(rounded = as.factor(as.integer(round((ride_time/n),0)*n))) %>%
       group_by(id, rounded) %>% arrange_(column) %>% slice(n()) %>% ungroup()
     rfReg <- loaded_data %>%
       select(ride_time, accumulated_power_watts, distance_m, altitude_meters, temperature_C,
              paste0(name, "_average"), paste0(name,"_cadence_average")
              , paste0(name, "_heartrate_average"), paste0(name,"_speed_average")
       ) %>%
       na.omit %>%
       arrange_(column) %>% tail(1000)
     rfFit <- randomForest(as.formula(paste0(column, " ~ .")), data =rfReg)
     varImportance <- importance(rfFit)
     varImportance <- data.frame(x = row.names(varImportance), y = varImportance[,1])
     ggplot(varImportance, aes(x = x, y = y)) + geom_bar(stat = 'identity', fill = "mediumaquamarine") +
       theme_minimal() +
       theme(text = element_text(size=20),
             axis.text.x = element_text(angle=90, hjust=1)) +
       labs(title = "Random Forest Gini Variable Importance") + ylab("Gini") + xlab("") 
       
   })
   # x<-reactive({(input$average_metric)})
   
   output$averagePlot <- renderPlot({ 
     # Speed
     # if (x() == 4) {
     #     curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(46:54)
     #     curve1 <- apply(curve1, 2, max, na.rm = T)
     #     curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(46:54)
     #     curve2 <- apply(curve2, 2, max, na.rm = T)
     #     curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
     #                          group = rep(c("Selection_1", "Selection_2"), each = 9),
     #                          max = append(as.vector(curve1), as.vector(curve2)))
     #     curves <- curves %>% mutate(duration = log(duration))
     #     ggplot(curves, aes(x = duration, y = max, color = group)) + geom_line() + geom_point() +
     #       ylab("Max Average Speed MPH") + xlab("Log of Duration, 5 sec to 1 hour")+
     #       theme_bw() + theme(legend.position="bottom")
     # }
     # 
     # # heart rate
     # if (x() == 2) {
     #     curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(37:45)
     #     curve1 <- apply(curve1, 2, max, na.rm = T)
     #     curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(37:45)
     #     curve2 <- apply(curve2, 2, max, na.rm = T)
     #     curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
     #                          group = rep(c("Selection_1", "Selection_2"), each = 9),
     #                          max = append(as.vector(curve1), as.vector(curve2)))
     #     curves <- curves %>% mutate(duration = log(duration))
     #     ggplot(curves, aes(x = duration, y = max, color = group)) + geom_line() + geom_point() +
     #       ylab("Max Average Heart Rate BPM") + xlab("Log of Duration, 5 sec to 1 hour") +
     #       theme_bw() + theme(legend.position="bottom")
     # }
     # # cadence
     # if (x() == 3) {
     #   curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(28:36)
     #   curve1 <- apply(curve1, 2, max, na.rm = T)
     #   curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(28:36)
     #   curve2 <- apply(curve2, 2, max, na.rm = T)
     #   curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
     #                        group = rep(c("Selection_1", "Selection_2"), each = 9),
     #                        max = append(as.vector(curve1), as.vector(curve2)))
     #   curves <- curves %>% mutate(duration = log(duration))
     #   ggplot(curves, aes(x = duration, y = max, color = group)) + geom_line() + geom_point() +
     #     ylab("Max Average Cadence RPM") + xlab("Log of Duration, 5 sec to 1 hour") +
     #     theme_bw() + theme(legend.position="bottom")
     # }
     # # power
     # if (x() == 1) {
     #   curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(19:27)
     #   curve1 <- apply(curve1, 2, max, na.rm = T)
     #   curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(19:27)
     #   curve2 <- apply(curve2, 2, max, na.rm = T)
     #   curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
     #                        group = rep(c("Selection_1", "Selection_2"), each = 9),
     #                        max = append(as.vector(curve1), as.vector(curve2)))
     #   curves <- curves %>% mutate(log.duration = log(duration))
     #   ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
     #     ylab("Max Average Power Watts") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour") +
     #     theme_bw() + theme(legend.position="bottom") + 
     #     geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
     # }
     # 
     # else {
     #   curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(46:54)
     #   curve1 <- apply(curve1, 2, max, na.rm = T)
     #   curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(46:54)
     #   curve2 <- apply(curve2, 2, max, na.rm = T)
     #   curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
     #                        group = rep(c("Selection_1", "Selection_2"), each = 9),
     #                        max = append(as.vector(curve1), as.vector(curve2)))
     #   curves <- curves %>% mutate(log.duration = log(duration))
     #   ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
     #     ylab("Max Average Speed MPH") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour")+
     #     theme_bw() + theme(legend.position="bottom")+
     #     geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
     # }
     # power
     curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(19:27)
     curve1 <- apply(curve1, 2, max, na.rm = T)
     curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(19:27)
     curve2 <- apply(curve2, 2, max, na.rm = T)
     curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                          group = rep(c(paste(year(input$date), "Selection 1"),paste(year(input$date3), "Selection 2")), each = 9),
                          max = append(as.vector(curve1), as.vector(curve2)))
     curves <- curves %>% mutate(log.duration = log(duration))
     g1 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
       ylab("Max Average Power Watts") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour") +
       theme_bw() + theme(legend.position="bottom") + 
       geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
     # Speed
     curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(46:54)
     curve1 <- apply(curve1, 2, max, na.rm = T)
     curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(46:54)
     curve2 <- apply(curve2, 2, max, na.rm = T)
     curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                          group = rep(c(paste(year(input$date), "Selection 1"),paste(year(input$date3), "Selection 2")), each = 9),
                          max = append(as.vector(curve1), as.vector(curve2)))
     curves <- curves %>% mutate(log.duration = log(duration))
     g2 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
       ylab("Max Average Speed MPH") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour")+
       theme_bw() + theme(legend.position="bottom")+
       geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
     # cadence
     curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(28:36)
     curve1 <- apply(curve1, 2, max, na.rm = T)
     curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(28:36)
     curve2 <- apply(curve2, 2, max, na.rm = T)
     curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                          group = rep(c(paste(year(input$date), "Selection 1"),paste(year(input$date3), "Selection 2")), each = 9),
                          max = append(as.vector(curve1), as.vector(curve2)))
     curves <- curves %>% mutate(log.duration = log(duration))
     g3 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
       ylab("Max Average Cadence RPM") + xlab("Log of Duration, Label in Seconds: 5 sec to 1 hour")+
       theme_bw() + theme(legend.position="bottom")+
       geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
     # heart rate
     curve1 <- garmin.data %>% filter(Date_GMT >= input$date, Date_GMT <= input$date2) %>% select(37:45)
     curve1 <- apply(curve1, 2, max, na.rm = T)
     curve2 <- garmin.data %>% filter(Date_GMT >= input$date3, Date_GMT <= input$date4) %>% select(37:45)
     curve2 <- apply(curve2, 2, max, na.rm = T)
     curves <- data.frame(duration = rep(c(5, 10, 30, 60, 180, 300, 600, 1800, 3600), times = 2),
                          group = rep(c(paste(year(input$date), "Selection 1"),paste(year(input$date3), "Selection 2")), each = 9),
                          max = append(as.vector(curve1), as.vector(curve2)))
     curves <- curves %>% mutate(log.duration = log(duration))
     g4 <- ggplot(curves, aes(x = log.duration, y = max, color = group, label = duration)) + geom_line() + geom_point() +
       ylab("Max Average Heart Rate") + xlab("Log of Duration, Labels in Seconds") +
       theme_bw() + theme(legend.position="bottom") + 
       geom_text(aes(label=duration),hjust=0, vjust=0, color = "gray")
     multiplot(g1, g2, g3, g4, cols=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

