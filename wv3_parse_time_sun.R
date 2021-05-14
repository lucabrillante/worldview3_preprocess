wv3_parse_time_sun <- function(path) {
  imd <- suppressWarnings(readLines(path))
  firstLineTime <- grep("firstLineTime = ", imd)
  earliestAcqTime  <- grep("earliestAcqTime = ", imd)
  if (length(earliestAcqTime) == 0) {
    time_line <- firstLineTime 
  } else {
    time_line <- earliestAcqTime
  }
  
  image_time <- strsplit(imd[time_line], " = ")[[1]][2]
  image_time <- strsplit(image_time, ";")[[1]][1]
  image_time <- as.POSIXlt(image_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  #This is the base R implementation, however results are slightly different from
  #Meeus p.61, 1998 that is implemented in the oce package, because R uses a different method
  #d <- as.double(julian(image_time) + 2440587.5) - 2451545
  
  d <- oce::julianDay(image_time) - 2451545
  g <- 357.529 + 0.98560028 * d
  des <- 1.00014 - 0.01671 * cos(g*pi/180) - 0.00014 * cos(2*g*pi/180)
  
  sun_el <- grep("meanSunEl", imd)
  sun_el <- readr::parse_number(imd[sun_el])
  sun_angle <- 90.0 - sun_el
  
  c("des" = des, "sun_el" = sun_el, "sun_angle" = sun_angle)

}
