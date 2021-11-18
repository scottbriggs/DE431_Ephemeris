
# Calculates the sin of the altitude of a body. Used to find the rise and set of
# bodies.
sin_alt <- function(hour_angle, obs_lat, dec)
{
  sinalt <- cos(obs_lat) * cos(dec) * cos(hour_angle) + sin(obs_lat) * sin(dec)
  
  return(sinalt)
}

# Event - Moon, Sun, Planet, Star
# JD - julian date
# FUN - Function to calculate the apparent place of the event
# obs_lat - Observer Latitude
# obs_long - Observer Longitude
rise_set_events <- function(event, jd, FUN, obs_lat, obs_long)
{
  sinh0 <- 0
  
  if (event == "Moon") {sinh0 <- 8/60 * DEG2RAD}
  if (event == "Sun") {sinh0 <- -50/60 * DEG2RAD}
  if (event == "Planet" | event == "Star") {sinh0 <- -34/60 * DEG2RAD}
  
  
}