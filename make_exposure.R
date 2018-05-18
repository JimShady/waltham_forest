rm(list = ls())

## Load libraries
library("googleway")
library("gepaf")
library("sp")
library('raster')
library('ggplot2')
library('geosphere')
library('sf')
library('reshape2')

exposure_routes <- read.csv('exposure_journeys.csv',
                            stringsAsFactors = F)

exposure_routes[exposure_routes$via_array == '',]$via_array <- NA

exposure_routes <- exposure_routes[exposure_routes$name == 'chingford_to_chingford_police',]

for (i in 1:nrow(exposure_routes)) {
#  for (i in 1:3) {

# Journey parameters
start_lat         <- exposure_routes[i,]$start_lat
start_lon         <- exposure_routes[i,]$start_lon
if (!is.na(exposure_routes[i,]$via_array)) {via_array         <- as.list(strsplit(as.character(exposure_routes[i,]$via_array), ";")[[1]]) } else {via_array <- NA}
end_lat           <- exposure_routes[i,]$end_lat
end_lon           <- exposure_routes[i,]$end_lon
mode              <- as.character(exposure_routes[i,]$mode)
start_time        <- Sys.time() + 1000
seggregated       <- exposure_routes[i,]$seggregated

## take account of where in the road the journey is taking place
if (mode == 'walk'    & seggregated == 'no')  {offset <- 6}
if (mode == 'walk'    & seggregated == 'yes') {offset <- 8}
if (mode == 'bicycle' & seggregated == 'no')  {offset <- 4}
if (mode == 'bicycle' & seggregated == 'yes') {offset <- 6}


# EPSG strings we might need
latlong                 <- "+init=epsg:4326"
ukgrid                  <- "+init=epsg:27700"
google_projected        <- "+init=epsg:3857 +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"

# Take the start/end data and get a route from Google
if ('walk' %in% mode)     { source("routing/walking.R") }
if ('bicycle' %in% mode)  { source("routing/cycling.R") }

rm(start_lat, start_lon, end_lat, end_lon, via_array)

if ('walk' %in% mode) {
  result <- walk_result
  rm(walk_result)
  duration <- walk_duration
  rm(walk_duration)
} else {
  result <- cycle_result
  rm(cycle_result)
  duration <- cycle_duration
  rm(cycle_duration)
}

##### In here is where we calculate the offset for where the journey is taking place compared to the centre of the road
##### https://stackoverflow.com/questions/50275195/draw-a-parallel-line-in-r-offset-from-a-line

#coordinates(result) <- ~lon + lat
#proj4string(result) =  CRS(latlong)
#result <- spTransform(result, ukgrid)

#segment.shift <- function(x, y, d){
#  
  # calculate vector
#  v <- c(x[2] - x[1],y[2] - y[1])
#  
  # normalize vector
#  v <- v/sqrt((v[1]**2 + v[2]**2))
#  
  # perpendicular unit vector
#  vnp <- c( -v[2], v[1] )
#  
#  return(list(x =  c( x[1] + d*vnp[1], x[2] + d*vnp[1]), 
#              y =  c( y[1] + d*vnp[2], y[2] + d*vnp[2])))
#  
#}
#
#x <- result@coords[,1]
#y <- result@coords[,2]
#
#xn <- numeric( (length(x) - 1) * 2 )
#yn <- numeric( (length(y) - 1) * 2 )
#
#for ( p in 1:(length(x) - 1) ) {
#  xs <- c(x[p], x[p+1])
#  ys <- c(y[p], y[p+1])
#  new.s <- segment.shift( xs, ys, offset)
#  xn[(p-1)*2+1] <- new.s$x[1] ; xn[(p-1)*2+2] <- new.s$x[2]
#  yn[(p-1)*2+1] <- new.s$y[1] ; yn[(p-1)*2+2] <- new.s$y[2]
#}

#new_result   <- data.frame(x = xn,
#                           y = yn)
#coordinates(new_result) <- ~x+y
#proj4string(new_result) = CRS(ukgrid)
#new_result              <- spTransform(new_result, latlong)
#new_result              <- data.frame(new_result)
#new_result$id           <- unique(result$id)
#new_result$line         <- unique(result$line)
#new_result$mode         <- unique(result$mode)

#result <- new_result
#rm(new_result, new.s, offset, x, xn, xs, y, yn, ys, segment.shift)
#names(result)[1:2] <- c('lon', 'lat')
##### End of the help from stackoverflow

coordinates(result) <- ~lon + lat
proj4string(result) = CRS(latlong)
result <- spTransform(result, ukgrid)

## This makes a spatialline from the points for the result
x <- lapply(split(result, result$id), function(x) Lines(list(Line(coordinates(x))), result$id[1L]))
lines <- SpatialLines(x)
data <- data.frame(id = unique(result$id))
rownames(data) <- data$id
result <- SpatialLinesDataFrame(lines, data)
proj4string(result) = CRS(ukgrid)
rm(x, lines, data)


## How many minutes long was the bicycle and walk journeys

timeslots                <- data.frame(id = unique(result$id), time = seq.POSIXt(start_time, start_time + duration, "min"))

## Now want to split the line into a point per minute

result                   <- data.frame(spsample(result, 
                                                nrow(timeslots),
                                                type="regular"))

## Now some harmonisation stuff to get the car, walk and bus all the same format so can join them together
result$id                 <- exposure_routes[i,]$journey_id
result$mode               <- mode
result$line               <- NA
result                    <- cbind(result, timeslots)
result                    <- result[,c('id', 'mode', 'line', 'time', 'lon', 'lat')]
result$scheme             <- exposure_routes[i,]$scheme
result$year               <- exposure_routes[i,]$year
coordinates(result)       <- ~lon + lat
proj4string(result)       <- CRS(ukgrid)

# Now get our pollutant files

## 2013 concentration maps
if (exposure_routes[i,]$year == 2013) {
wf_2013_no2              <- raster('air_quality/no2_2013.asc')
crs(wf_2013_no2)         <- CRS(ukgrid)
wf_2013_nox              <- raster('air_quality/nox_2013.asc')
crs(wf_2013_nox)         <- CRS(ukgrid)
wf_2013_pm10             <- raster('air_quality/pm10_2013.asc')
crs(wf_2013_pm10)        <- CRS(ukgrid)
wf_2013_pm25             <- raster('air_quality/pm25_2013.asc')
crs(wf_2013_pm25)        <- CRS(ukgrid)

# Extract concentrations
no2                                   <- result
no2@data$pollutant                    <- 'no2'
no2@data$concentration                <- extract(wf_2013_no2, no2@coords)

pm25                                  <- result
pm25@data$pollutant                   <- 'pm25'
pm25@data$concentration               <- extract(wf_2013_pm25, pm25@coords)

pm10                                  <- result
pm10@data$pollutant                   <- 'pm10'
pm10@data$concentration               <- extract(wf_2013_pm10, pm10@coords)

nox                                   <- result
nox@data$pollutant                    <- 'nox'
nox@data$concentration                <- extract(wf_2013_nox, nox@coords)

result                                <- rbind.SpatialPointsDataFrame(no2, pm25, nox, pm10)

rm(wf_2013_no2, wf_2013_nox, wf_2013_pm10, wf_2013_pm25,nox, no2, pm25, pm10)

}

## 2021 concentration maps without schemes
if (exposure_routes[i,]$year == 2021 & exposure_routes[i,]$scheme == 'none') {
wf_2021_no2              <- raster('air_quality/no2_2021.asc')
crs(wf_2021_no2)         <- CRS(ukgrid)
wf_2021_nox              <- raster('air_quality/nox_2021.asc')
crs(wf_2021_nox)         <- CRS(ukgrid)
wf_2021_pm10             <- raster('air_quality/pm10_2021.asc')
crs(wf_2021_pm10)        <- CRS(ukgrid)
wf_2021_pm25             <- raster('air_quality/pm25_2021.asc')
crs(wf_2021_pm25)        <- CRS(ukgrid)


# Extract concentrations
no2                                   <- result
no2@data$pollutant                    <- 'no2'
no2@data$concentration                <- extract(wf_2021_no2, no2@coords)

pm25                                  <- result
pm25@data$pollutant                   <- 'pm25'
pm25@data$concentration               <- extract(wf_2021_pm25, pm25@coords)

pm10                                  <- result
pm10@data$pollutant                   <- 'pm10'
pm10@data$concentration               <- extract(wf_2021_pm10, pm10@coords)

nox                                   <- result
nox@data$pollutant                    <- 'nox'
nox@data$concentration                <- extract(wf_2021_nox, nox@coords)

result                                <- rbind.SpatialPointsDataFrame(no2, pm25, nox, pm10)

rm(wf_2021_no2, wf_2021_nox, wf_2021_pm10, wf_2021_pm25,nox, no2, pm25, pm10)

}

## 2021 concentration maps with schemes
if (exposure_routes[i,]$year == 2021 & exposure_routes[i,]$scheme == 'implemented') {
wf_2021_no2_s            <- raster('air_quality/no2_2021_s.asc')
crs(wf_2021_no2_s)       <- CRS(ukgrid)
wf_2021_nox_s            <- raster('air_quality/nox_2021_s.asc')
crs(wf_2021_nox_s)       <- CRS(ukgrid)
wf_2021_pm10_s           <- raster('air_quality/pm10_2021_s.asc')
crs(wf_2021_pm10_s)      <- CRS(ukgrid)
wf_2021_pm25_s           <- raster('air_quality/pm25_2021_s.asc')
crs(wf_2021_pm25_s)      <- CRS(ukgrid)

# Extract concentrations
no2                                   <- result
no2@data$pollutant                    <- 'no2'
no2@data$concentration                <- extract(wf_2021_no2_s, no2@coords)

pm25                                  <- result
pm25@data$pollutant                   <- 'pm25'
pm25@data$concentration               <- extract(wf_2021_pm25_s, pm25@coords)

pm10                                  <- result
pm10@data$pollutant                   <- 'pm10'
pm10@data$concentration               <- extract(wf_2021_pm10_s, pm10@coords)

nox                                   <- result
nox@data$pollutant                    <- 'nox'
nox@data$concentration                <- extract(wf_2021_nox_s, nox@coords)

result                                <- rbind.SpatialPointsDataFrame(no2, pm25, nox, pm10)

rm(wf_2021_no2_s, wf_2021_nox_s, wf_2021_pm10_s, wf_2021_pm25_s,nox, no2, pm25, pm10)

}

if (i == 1) {final_result <- result} else {final_result <- rbind.SpatialPointsDataFrame(final_result, result)}

rm(timeslots, duration, start_time, result)

}

final_result$time <- NULL
final_result$line <- NULL
final_result      <- merge(final_result, exposure_routes[,c('journey_id', 'name')], by.x = 'id', by.y = 'journey_id')
rm(i, google_projected, latlong, mode, p, seggregated, ukgrid)

#####################
## something odd going on with Cycling coppermill_to_wood_street. Not 3 times the route. Should be 60, 60, 60, 180. Not 132.
## Similar with leytonstone_to_stratford_drapers

#plot(final_result[final_result$name == 'coppermill_to_wood_street',])





aggregate(concentration ~ name + mode + scheme + year + pollutant, data=final_result, FUN=mean)

plot(extent(final_result) * 1.1)

plot(final_result[final_result$year == '2013' & final_result$pollutant == 'no2',], add=T)
plot(final_result[final_result$year == '2021' & final_result$pollutant == 'no2' & final_result$scheme == 'none',], add=T, col='red')
plot(final_result[final_result$year == '2021' & final_result$pollutant == 'no2' & final_result$scheme == 'implemented',], add=T, col='blue')

#for (i in 1:length(unique(final_result$name))) {
#  
#  scheme_name <- unique(final_result$name)[i]
#  
#  ggplot(final_result[final_result$name == scheme_name,]@data, aes(x = as.factor(year), y = concentration, fill = scheme)) +
#    geom_boxplot() +
#    facet_wrap(name~pollutant, ncol = 4, scales = 'free_y') +
#    xlab('Year') +
#  
#}


