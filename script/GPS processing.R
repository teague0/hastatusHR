#GPS processing of 2023 P. hastatus tags
library(tidyverse)
library(move)
library(sf)
library(lubridate)

#Get GPS data & calculate the needed variables to get to airspeed
gps <- read.csv("./rawData/GPS/BATS_GPS.csv")
gps$TimestampUTC <- dmy_hms(gps$Timestamp, tz = "UTC")
gps$TimestampLocal <- with_tz(gps$TimestampUTC, tz = "America/Panama")
gps_m <- move(gps$location.lon, 
              y = gps$location.lat,
              time = gps$TimestampUTC,
              proj = CRS("+proj=longlat +ellps=WGS84"),
              animal = gps$TagID,
              data = gps)
gps_m$heading <- unlist(lapply(lapply(split(gps_m), angle), c, NA))
gps_m$timelag <- unlist(lapply(timeLag(gps_m, units="secs"), c, NA))
gps_m$ground.speed_locs <- unlist(lapply(speed(gps_m), c, NA)) #calculate ground speed between locations. How does this compare to on-tag calculations?
gps_m$stepLength <- unlist(lapply(distance(gps_m), c, NA))
gps_m$angle <- unlist(lapply(angle(gps_m), c, NA))
gps_m$batID <- gps_m@trackId
gps_df$ground.speed <- gps_df$ground.speed * 1000/3600 #km/h to m/s. These speeds are wrong and overestimate ground speed. DO NOT USE FOR AIRSPEED CALCULATIONS

ggplot(gps_m@data)+
  geom_point(aes(x = ground.speed, y = ground.speed_locs))

#Add in Bat Day
moveList <- lapply(split(gps_m), function(myInd){
  datechange <- c(0, abs(diff(as.numeric(as.factor(date(myInd@timestamps-(12*60*60)))))))
  myInd$BatDay <- cumsum(datechange)+1
  return(myInd)
})
gps_m <- moveStack(moveList, forceTz="UTC") #original GPS times are in UTC. It's kept this way & the local time is in the @dataframe slot

#The move object can go back to a dataframe now.

gps_df <- as.data.frame(gps_m)
gps_df$BatIDday <- paste0(gps_df$batID, "_d", gps_df$BatDay)


#Weather data from Steve Paton at STRI. Values are 15 minute averages in km/h
windDir <- read_csv("./rawData/weather/bocas_wd_avg.csv")
windDir$TimestampLocal <- dmy_hms(windDir$datetime, tz = "America/Panama")
windSpd <- read_csv("./rawData/weather/bocas_ws_avg.csv")
windSpd$TimestampLocal <- dmy_hms(windSpd$datetime, tz = "America/Panama")

wind <- windSpd %>% left_join(windDir)

#wind speed & direction needs to be converted to the U & V vectorial components of wind (http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv)
wind$ws.ms <- wind$ws * 1000/3600 #kmh to ms

#Calculate U & V components of wind
library(circular)
wind$U.Component <-  -wind$ws.ms*sin(rad(wind$wdvm)) 
wind$V.Component <-  -wind$ws.ms*cos(rad(wind$wdvm))


#Bocas data were given as average for his 15 minute intervals but the GPS data are in 1 min. Interpolate to get weighted weather values.
t <- unique(gps$TimestampLocal)
t.fl <- data.frame(floor_date(t, unit = "15 mins"))
names(t.fl) <- c("TimestampLocal")
pre <- left_join(t.fl, wind)
t.ce <- data.frame(ceiling_date(t, unit = "15 mins"))
names(t.ce) <- c("TimestampLocal")
post <- left_join(t.ce, wind)

Udif <- post$U.Component - pre$U.Component
Vdif <- post$V.Component - pre$V.Component
dtif <- as.numeric(t - pre$TimestampLocal)
wt <- dtif / 3600
U.Component <- pre$U.Component + Udif * wt
V.Component <- pre$V.Component + Vdif * wt
timestamp <- t
winddat <- data.frame(timestamp, U.Component, V.Component)

#Join the wind date to the GPS data

gps_df <- gps_df %>% left_join(winddat, by = c("TimestampLocal" = "timestamp"))

#I also want to set the first ground.speed_locs to NA for each night. Since that ground speed was calculated & added to the second point for each segment, there can be connections between the last point of the night 1 and the first point of night 2.

gpsX <- lapply(split(gps_df, f = gps_df$BatIDday), function(dayNA){
  dayNA$ground.speed_locs[1] <- NA
  return(dayNA)
})
gps_df <- rbindlist(gpsX)


##### Calculate airspeed

#To calculate airspeed we need to combine the ground speed of the bat with the wind support (poth positive -- tail wind, and negative -- head wind) that the bat has. This includes the crosswinds from any direction. 

wind_support <- function(u,v,heading) {
  angle <- atan2(u,v) - heading/180*pi
  return(cos(angle) * sqrt(u*u+v*v))
}

cross_wind <- function(u,v,heading) {
  angle <- atan2(u,v) - heading/180*pi
  return(sin(angle) * sqrt(u*u+v*v))
}

#ground.speed_locs that was calculated between points and cleaned. The exact ground speed calculation can always be changed.
airspeed <- function(x)
{
  va <- sqrt((x$ground.speed_locs - x$ws)^2 + (x$cw)^2)
  return(va)
}

#This will split the GPS data by bat day so that everything is calculated per bat day so that we don't have any connections between last fix of day 1 & first fix of day 2
gpsL <- split(gps_df, f = gps_df$BatIDday)
gpsLX <- lapply(gpsL, function(x){
  df <- data.frame(x)
  x$ws <- wind_support(df$U.Component, df$V.Component, df$heading)
  x$cw <- cross_wind(df$U.Component, df$V.Component, df$heading)
  x$airspeed <- airspeed(x)
  return(x)
}) 

gps_df <- rbindlist(gpsLX)

write.csv(gps_df, file = "./processedData/GPS_allbats20240418.csv", row.names = FALSE)

hist(gps_df$airspeed, breaks="FD")
#From O'Mara & Dechmann 2023:
#minimum power: 6.81 ± 0.21 m/s
#maximum range: 11.0 ± 0.35 m/s

#There are a couple of blips above 20 m/s of airspeeds. There is likely an error in the GPS locations for those speeds and we need to look at them closely to see if they need to be cleaned out of the data set. It's a normal part of processing GPS.

ggplot(gps_df)+
  geom_density(aes(x = airspeed, fill = batID))+
  facet_wrap(~batID, scales = "free_y")

