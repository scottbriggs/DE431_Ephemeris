
topocentric_place_mercury <- function(jd, lat, long, height)
{
  # Observer position vector
  obs_pos_vec <- observer_position_vector(lat, long, height)
  
  # Greenwich apparent sidereal time
  gmst <- sidereal_time(jd)
  
  # Geocentric position and velocity of the observer
  obs_geo_pos <- rotation_matrix(3, gmst[[2]] * HR2RAD) %*% obs_pos_vec
  obs_geo_vel <- ROTANGVELEARTH * obs_geo_pos

  # Convert to AU and AU/day
  obs_geo_pos <- obs_geo_pos / 1.49597870e11
  obs_geo_vel <- obs_geo_vel * (86400.0 / 1.49597870e11)
  
  # Get the precession matrix
  precess <- precession_matrix(jd)
  
  # Get the nutation matrix
  nut <- nutation_matrix(jd)
  
  # Transform the observer vectors to the mean equator and equinox of the
  # reference epoch. Need to calculate the inverse or the transpose of the
  # precession and nutation matrices
  G <- (t(precess) * t(nut)) %*% obs_geo_pos
  G_dot <- (t(precess) * t(nut)) %*% obs_geo_vel
  
  # Calculate the number of julian centuries since the epoch of observation
  T_prime <- (jd - EPOCHJ2000) / DAYSJULCENT
  
  # Calculate the mean anomaly of the Earth in its orbit, in radians, at the
  # epoch of observation
  mean_anomaly <- (357.528 + 35999.050 * T_prime) * PI2 / 360
  
  # Calculate the difference TDB - TDT
  s <- 0.001658 * sin(mean_anomaly + 0.01671 * sin(mean_anomaly))
  t <- jd + s / 86400
  
  T <- (t - EPOCHJ2000) / DAYSJULCENT
  
  # Extract the barycentric position and velocity of the Earth and convert to AU
  # and AU/day
  earth_ssb <- position_earth_ssb(t)
  earth_ssb_au <- earth_ssb[,1] / KM2AU
  earth_ssb_au_day <- earth_ssb[,2] / KM2AU
  
  # Add the observer position and velocity vectors to the earth SSB vectors
  earth_ssb_au <- earth_ssb_au + G
  earth_ssb_au_day <- earth_ssb_au_day + G_dot
  
}