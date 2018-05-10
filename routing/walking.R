source('../google_api_key.R')

if (is.na(via_array[[1]])) {
  
  ## If there are no waypoints
  json_data <- google_directions(origin = paste(start_lat, ",", start_lon, sep="") ,
                                 destination = paste(end_lat, ",", end_lon, sep=""),
                                 key = google_api_key,
                                 mode = "walking",
                                 departure_time = start_time,
                                 simplify = T)
} else {
  
  ## if there are waypoints
  json_data <- google_directions(origin = paste(start_lat, ",", start_lon, sep="") ,
                                 destination = paste(end_lat, ",", end_lon, sep=""),
                                 waypoints = via_array,
                                 key = google_api_key,
                                 mode = "walking",
                                 departure_time = start_time,
                                 simplify = T)
}

if (json_data$status == 'OK') {
    walk_result <- data.frame(  id = 2,
                           lat = decodePolyline(json_data$routes$overview_polyline$points)$lat,
                           lon = decodePolyline(json_data$routes$overview_polyline$points)$lon,
                           mode = 'Walk',
                           line = NA,
                           stringsAsFactors = FALSE)
    
    walk_duration = sum(json_data$routes$legs[[1]]$duration$value)
    
  rm(json_data, google_api_key)
  
  print(paste("Walk routing was completed using Google Directions API"))
  } else {
      print(paste("Routing failed"))
}
