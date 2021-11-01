
topocentric_place_mercury <- function(jd, lat, long, height)
{
  # Observer position vector
  obs_pos_vec <- observer_position_vector(lat, long, height)
  
  # Greenwich apparent sidereal time
  gmst <- sidereal_time(jd)
  
  # Geocentric position and velocity of the observer
  obs_geo_pos <- rotation_matrix(3, -gmst[[2]] * HR2RAD) %*% obs_pos_vec
  temp <- matrix(0.0,nrow=3,ncol=3)
  temp[1,1] <- -sin(gmst[[2]] * HR2RAD)
  temp[1,2] <- -cos(gmst[[2]] * HR2RAD)
  temp[2,1] <- -temp[1,2]
  temp[2,2] <- temp[1,1]
  obs_geo_vel <- ROTANGVELEARTH * temp %*% obs_pos_vec

  # Convert to AU and AU/day
  obs_geo_pos <- obs_geo_pos / M2AU
  obs_geo_vel <- obs_geo_vel * (SEC2DAY / M2AU)
  
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
  
  # Calculate the mean anomaly of the Earth in its orbit in radians, at the
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
  
  # Extract the barycentric position and velocity of the Sun and convert to AU
  # and AU/day
  sun_ssb <- position_sun_ssb(t)
  sun_ssb_au <- sun_ssb[,1] / KM2AU
  
  # Create the heliocentric position of the Earth and convert to AU and AU/day
  helio_earth <- earth_ssb - sun_ssb
  helio_earth_au <- helio_earth[,1] / KM2AU
  
  # Extract the barycentric position and velocity of Mercury and convert to AU
  # and AU/day
  mercury_ssb <- position_mercury_ssb(t)
  mercury_ssb_au <- mercury_ssb[,1] / KM2AU
  
  # Calculate the geometric distance between the positions of the center of mass
  # of Mercury and the Earth
  tmp <- mercury_ssb_au - earth_ssb_au
  d <- magnitude(tmp)
  
  # Calculate a first approximation to the light-travel time between Mercury
  # and the Earth
  tau = d / CAUD
  
  mercury_ssb <- position_mercury_ssb(t-tau)
  mercury_ssb_au <- mercury_ssb[,1] / KM2AU
  
  sun_ssb <- position_sun_ssb(t-tau)
  sun_ssb_au <- sun_ssb[,1] / KM2AU
  
  U <- mercury_ssb_au - earth_ssb_au
  Q <- mercury_ssb_au - sun_ssb_au
  
  tau_prime <- magnitude(U) / CAUD
  
  mercury_ssb <- position_mercury_ssb(t-tau_prime)
  mercury_ssb_au <- mercury_ssb[,1] / KM2AU
  
  sun_ssb <- position_sun_ssb(t-tau_prime)
  sun_ssb_au <- sun_ssb[,1] / KM2AU
  
  U <- mercury_ssb_au - earth_ssb_au
  Q <- mercury_ssb_au - sun_ssb_au
  
  # Calculate the relativistic deflection of light
  E_mag <- magnitude(helio_earth_au)
  U_uv <- unit_vector(U)
  Q_uv <- unit_vector(Q)
  E_uv <-unit_vector(helio_earth_au)
  g1 <- MUC/E_mag
  g2 <- 1 + dot_product(Q_uv, E_uv)
  dot_UQ <- dot_product(U_uv, Q_uv)
  dot_EU <- dot_product(E_uv, U_uv)
  
  U1 <- magnitude(U) * (U_uv + (g1/g2 * ((dot_UQ*E_uv) - (dot_EU*Q_uv))))
  
  # Calculate the aberration of light
  U2 <- U1 + magnitude(U1) * (earth_ssb_au_day / CAUD)
  
  # Calculate and apply the precession matrix
  U3 <- precess %*% U2
  
  # Calculate and apply the nutation matrix
  U4 <- nut %*% U3
  
  # Calculate the right ascension and declination
  right_asc <- ra(U4)
  declination <- dec(U4)
  
  return (list(v1 = right_asc, v2 = declination, v3 = d))
  
}