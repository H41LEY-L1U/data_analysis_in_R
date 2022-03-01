library(rgdal)
library(spData)
library(gmt)
library(measurements)
library(leaflet)
library(maps)
library(tmap)
library(pacman)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(gridExtra)
register_google(key = "AIzaSyC_giPfJ6QgBVGc5O1hN82gy2gtsXj0s6g") 

#1.L.A. Chloropleth using rgdal
LA <- readOGR('Los_Angeles', 'los_angeles')
freeways <- readOGR("los_angeles", "los_angeles_freeways")

plot(LA)
plot(freeways, col = "red", add = T)

names(LA)

LA$hispanc # just a list of population numbers that correlate with tractID
LA$tractID
par(mfrow = c(2,2))
sp1 <- spplot(LA, "PrCpInc", 
              main = "Los Angeles Demographics", 
              sub = "Average Per Capita Income", 
              col = "transparent")
sp2 <- spplot(LA, "black", 
              main = "Los Angeles Demographics", 
              sub = "Average Black Population", 
              col = "transparent")
sp3 <- spplot(LA, "hispanc", 
              main = "Los Angeles Demographics", 
              sub = "Average Hispanic Population", 
              col = "transparent")
sp4 <- spplot(LA, "White", 
              main = "Los Angeles Demographics", 
              sub = "Average White Population", 
              col = "transparent")

grid.arrange(sp1, sp2, sp3, sp4)

#2. Custom Chloropleth with Tmap, Leaflet and Geospatial Distance
london <- c(51.507222, -0.1275)
chicago <- c(41.881944, -87.627778)
mydist <- geodist(london[1], london[2], chicago[1], chicago[2], "km")
mydist

conv_unit(mydist, "km", "mi")  

?world 
?map
pacman::p_load(tmap, statR, tidyverse, sp, cartogram)
data(metro, World)
tm_shape(World)+tm_polygons("income_grp", palette="-Blues", contrast=.7, id="name", title="Income Index")

#3.Chicago crime demographics – Heatmaps (ggmap or leaflet.extras)
searchTerm <- "CRIMINAL SEXUAL ASSAULT"
chicagoMVT_generated <- subset(chicagoCrimes, Primary.Type == searchTerm)
chicagoMVT <- chicagoMVT_generated

#Sys.setlocale("LC_TIME", "C")
chicagoMVT$Date <- strptime(chicagoMVT$Date, format = '%m/%d/%Y %I:%M:%S %p')
chicagoMVT$Day <- weekdays(chicagoMVT$Date)
chicagoMVT$Hour <- chicagoMVT$Date$hour
dailyCrimes <- as.data.frame(table(chicagoMVT$Day, chicagoMVT$Hour))
names(dailyCrimes) <- c('Day', 'Hour', 'Freq')
dailyCrimes$Hour <- as.numeric(as.character(dailyCrimes$Hour))
dailyCrimes$Day <- factor(dailyCrimes$Day, ordered = TRUE, 
                          levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
ggplot(dailyCrimes, aes(x = Hour, y = Freq)) + geom_line(aes(group = Day, color = Day)) + xlab('Hour') + ylab('Number') + ggtitle(paste('Daily number of ',searchTerm, sep =''))

chicagoMVT$Location[chicagoMVT$Location == ''] <- NA
chicagoMVT <- na.omit(chicagoMVT)

chicagoMVT <- chicagoMVT %>% extract(Location, c('Latitude', 'Longitude'), '\\(([^,]+), ([^)]+)\\)')
chicagoMVT$Longitude <- round(as.numeric(chicagoMVT$Longitude), 2)
chicagoMVT$Latitude <- round(as.numeric(chicagoMVT$Latitude), 2)

locationCrimes <- as.data.frame(table(chicagoMVT$Longitude, chicagoMVT$Latitude))
names(locationCrimes) <- c('long', 'lat', 'Frequency')
locationCrimes$long <- as.numeric(as.character(locationCrimes$long))
locationCrimes$lat <- as.numeric(as.character(locationCrimes$lat))
locationCrimes <- subset(locationCrimes, Frequency > 0)

ggmap(chicago) + ggtitle(paste('Heatmap of ',searchTerm, sep ='')) + 
  geom_tile(data = locationCrimes, aes(x = long, y = lat, alpha = Frequency),
            fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())
