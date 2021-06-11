# Calculate the apparent place of a planet taking into account precession,
# nutation, gravitational light deflection, and the aberration of light

apparent_place <- function(con, jd, body)
{
  # Calculate the number of julian centuries since the epoch of observation
  T_prime <- (jd - 2451545.0) / 36525
  
  # Calculate the mean anomaly of the Earth in its orbit, in radians, at the
  # epoch of observation
  PI2 <- 6.283185307179586476925287
  mean_anomaly <- (357.528 + 35999.050 * T_prime) * PI2 / 360
  
  # Calculate the difference TDB - TDT
  s <- 0.001658 * sin(mean_anomaly + 0.01671 * sin(mean_anomaly))
  t <- jd + s / 86400
  
  T <- (t - 2451545.0) / 36525
  
  # Extract the barycentric position and velocity of the Earth
  earth_ssb <- position_earth_ssb(con, t)
  
  # Extract the barycentric position and velocity of the Sun
  sun_ssb <- position_body_ssb(con, t, 'Sun')
  
  # Create the heliocentric position of the Earth
  helio_earth <- earth_ssb - sun_ssb
  
  # Extract the barycentric position and velocity of the planet
  body_ssb <- position_body_ssb(con, t, body)
  
  # Convert data frames from km and km/day to AU and AU/day
  earth_ssb_au <- km_sec2AU_day(earth_ssb)
  helio_earth_au <- km_sec2AU_day(helio_earth)
  body_ssb_au <- km_sec2AU_day(body_ssb)
  
  # Calculate the geometric distance between the positions of the center of mass
  # of the body and the Earth
  tmp <- body_ssb_au - earth_ssb_au
  d <- magnitude(tmp[,1])
  
  # Calculate a first approximation to the light-travel time between the body
  # and the Earth
  tau = d / 173.144633
  
  
}