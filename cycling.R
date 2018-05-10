google_api_key       <- 'AIzaSyDeuJbS1ht7H5oO1pUoLWKjficxNgp54nY'

if (is.na(via_array[[1]])) {

## If there are no waypoints
  json_data <- google_directions(origin = paste(start_lat, ",", start_lon, sep="") ,
                  destination = paste(end_lat, ",", end_lon, sep=""),
                  key = google_api_key,
                  mode = "bicycling",
                  departure_time = start_time,
                  simplify = T)
} else {
  
  ## if there are waypoints
  json_data <- google_directions(origin = paste(start_lat, ",", start_lon, sep="") ,
                  destination = paste(end_lat, ",", end_lon, sep=""),
		              waypoints = via_array,
                  key = google_api_key,
                  mode = "bicycling",
                  departure_time = start_time,
                  simplify = T)
}

## Having got the data, check the route is ok

if (json_data$status == 'OK') {
    cycle_result <- data.frame(  id = 4,
                           lat = decodePolyline(json_data$routes$overview_polyline$points)$lat,
                           lon = decodePolyline(json_data$routes$overview_polyline$points)$lon,
                           mode = 'Cycle',
                           line = NA,
                           stringsAsFactors = FALSE)
    
    cycle_duration = sum(json_data$routes$legs[[1]]$duration$value)
    
  rm(json_data, google_api_key)
  
  print(paste("Cycle routing was completed using Google Directions API"))
  } else {
      print(paste("Routing failed"))
}
