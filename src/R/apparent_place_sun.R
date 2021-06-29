

apparent_place_sun <- function(jd)
{
  # Calculate the number of julian centuries since the epoch of observation
  T_prime <- (jd - 2451545.0) / 36525
  
  # Calculate the mean anomaly of the Earth in its orbit, in radians, at the
  # epoch of observation
  mean_anomaly <- (357.528 + 35999.050 * T_prime) * PI2 / 360
  
  # Calculate the difference TDB - TDT
  s <- 0.001658 * sin(mean_anomaly + 0.01671 * sin(mean_anomaly))
  t <- jd + s / 86400
  
  T <- (t - 2451545.0) / 36525
  
  # Extract the barycentric position of the Earth
  earth_ssb <- position_earth_ssb(t)
  earth_ssb_au <- earth_ssb[,1] / KM2AU
  earth_ssb_au_day <- earth_ssb[,2] / KM2AU

  # Extract the barycentric position of the Sun
  sun_ssb <- position_sun_ssb(t)
  sun_ssb_au <- sun_ssb[,1] / KM2AU
  
  # Calculate the geometric distance between the positions of the center of mass
  # of the Sun and the Earth
  sun_geo_au <- sun_ssb_au - earth_ssb_au
  d <- magnitude(sun_geo_au)
  
  # Calculate a first approximation to the light-travel time between the Sun
  # and the Earth
  tau = d / CAUD
  
  sun_ssb <- position_sun_ssb(t - tau)
  sun_ssb_au <- sun_ssb[,1] / KM2AU
  
  sun_geo_au <- sun_ssb_au - earth_ssb_au
  mag_sun_geo_au <- magnitude(sun_geo_au)
  U2 <- sun_geo_au + mag_sun_geo_au * (earth_ssb_au_day / CAUD)
  
  # Calculate and apply the precession matrix
  prec <- precession_matrix(t)
  U3 <- prec %*% U2
  
  # Calculate and apply the nutation matrix
  nut <- nutation_matrix(t)
  U4 <- nut %*% U3
  
  # Calculate the right ascension and declination
  res <- ra_dec(U4)
  res[3,1] <- d
  
  return(res) 
}