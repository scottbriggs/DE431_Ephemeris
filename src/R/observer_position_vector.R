
# Calculate the observer position vector using the geodetic latitude (phi),
# geodetic longitude (lambda), and the height above sea level
# Phi is measured positive north of the equator and negative south of the equator in degrees
# Lambda is positive for east longitudes and negative for west longitudes in degrees
# Height is in meters

observer_position_vector <- function(lat, long, height)
{
  f <- (1-FLAT)*(1-FLAT)
  coslat <- cos(lat*DEG2RAD)
  cos2lat <- coslat * coslat
  sinlat <- sin(lat*DEG2RAD)
  sin2lat <- sinlat * sinlat
  C <- 1/sqrt(cos2lat + f * sin2lat)
  S <- f * C
  h <- (height / 1000.0) / KM2AU
  
  pos_obs <- c(0.0, 0.0, 0.0)
  pos_obs[1] <- (EARTHRADAU * C + h) * coslat * cos(long*DEG2RAD)
  pos_obs[2] <- (EARTHRADAU * C + h) * coslat * sin(long*DEG2RAD)
  pos_obs[3] <- (EARTHRADAU * S + h) * sinlat
  
  return(pos_obs)
}