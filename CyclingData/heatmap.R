library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(leaflet)
library(leaflet.extras)

# load data, cleaned .fit files save as csv then as RDA
garmin.data <- readRDS("GarminData.rda") #6129336 obs of 34 variables
# Select only relevant columns
garmin.data <- garmin.data[garmin.data$Type == "Data",c(1:13,34)]
garmin.data <- garmin.data %>% filter(!is.na(Latitude))

######### Leaflet

#Initially 4755390 obs of 15 variables
# process just the first coordinates for each ride, and group by location group
leaflet.data <- garmin.data %>% group_by(id) %>% slice(1) %>% ungroup() %>%
  mutate(Latitude = round(Latitude, 3),
                                       Longitude = round(Longitude, 3)) %>% 
  group_by(Latitude, Longitude) %>% 
  count() 

# further refine MN and Colorado points
leaflet.data2 <- rbind(leaflet.data[leaflet.data$n<20,],data.frame(Latitude = c(44.979, 38.843), Longitude = c(-93.235,-104.795), n = c(333,121)))

# saveRDS(leaflet.data, "leaflet.data.rda")

map1 <- leaflet(leaflet.data) %>% addProviderTiles(providers$CartoDB.DarkMatter)  %>% addMarkers( #, size = 15, units = "px"
    clusterOptions = markerClusterOptions()
  ) # intensity =~n
map1
# library(htmlwidgets)
# saveWidget(map1, "map.html")

# Attempt at a proper 'heatmap'
# Source: https://www.r-bloggers.com/where-do-you-run-to-map-your-strava-activities-on-static-and-leaflet-maps/
# Arkansas only because of limited size capabilities of leaflet
# arkansas <- garmin.data %>% filter(Longitude < -90, Longitude > -100,
#                                     Latitude < 40, Latitude > 30) %>%
#   select(id, Latitude, Longitude) %>% mutate(Latitude = round(Latitude, 4),
#                                              Longitude = round(Longitude, 4)) %>% 
#   group_by(Latitude, Longitude) %>% 
#   head(1500)
#   #count() 
# map <- leaflet(arkansas) %>% addProviderTiles(providers$CartoDB.DarkMatter) 
# map <-  addPolylines(map, lng = arkansas$Longitude, lat = arkansas$Latitude,
#                      group = arkansas$id,
#                      color = "red", opacity = 1/3, weight = 2)
# map #issue is it connects all rides together.


######## Regular Map

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() + mapWorld 


myPalette <- colorRampPalette(rev(brewer.pal(5, "YlOrRd")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 500))
sc2 <- scale_colour_gradient2(low = "purple3", high = "red2", midpoint = 60,limits=c(1, 335))

mp <- mp + geom_point(data=leaflet.data2, aes(x=Longitude, y=Latitude, color=n,  size = n)) + sc
mp <- mp + ggtitle("Kelly Catlin Ride Locations March 2015 - March 2018")
mp
# png('map3.png', units="in", width=8.75, height=5, res=600)
# mp
# dev.off()
