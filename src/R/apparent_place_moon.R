
apparent_place_moon <- function(jd)
{
  # Calculate the number of julian centuries since the epoch of observation
  T_prime <- (jd - EPOCHJ2000) / DAYSJULCENT
  
  # Calculate the mean anomaly of the Earth in its orbit, in radians, at the
  # epoch of observation
  mean_anomaly <- (357.528 + 35999.050 * T_prime) * PI2 / 360
  
  # Calculate the difference TDB - TDT
  s <- 0.001658 * sin(mean_anomaly + 0.01671 * sin(mean_anomaly))
  t <- jd + s / 86400
  
  T <- (t - EPOCHJ2000) / DAYSJULCENT
  
  # Extract the barycentric position and velocity of the Earth
  earth_ssb <- position_earth_ssb(t)
  earth_ssb_au <- earth_ssb[,1] / KM2AU
  earth_ssb_au_day <- earth_ssb[,2] / KM2AU
  
  # Extract the geocentric position and velocity of the Moon
  moon_geo <- position_moon_geo(t)
  moon_geo_au <- moon_geo[,1] / KM2AU
  moon_geo_au_day  <- moon_geo[,2] / KM2AU
  
  d <- magnitude(moon_geo_au)
  tau <- d / CAUD
  
  moon_geo <- position_moon_geo(t - tau)
  moon_geo_au <- moon_geo[,1] / KM2AU
  moon_geo_au_day <- moon_geo[,2] / KM2AU
  
  # Calculate the aberration of light
  U2 <- moon_geo_au + magnitude(moon_geo_au) * (earth_ssb_au_day / CAUD)

  # Calculate and apply the precession matrix
  prec <- precession_matrix(t)
  U3 <- prec %*% U2

  # Calculate and apply the nutation matrix
  nut <- nutation_matrix(t)
  U4 <- nut %*% U3
  
  return (U4)
}