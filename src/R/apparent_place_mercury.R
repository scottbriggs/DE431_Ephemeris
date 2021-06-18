
apparent_place_mercury <- function(con, jd)
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
  
  # Extract the barycentric position and velocity of the Earth
  earth_ssb <- position_earth_ssb(con, t)
  
  # Extract the barycentric position and velocity of the Sun
  sun_ssb <- position_sun_ssb(con, t)
  
  # Create the heliocentric position of the Earth
  helio_earth <- earth_ssb - sun_ssb
  
  # Extract the barycentric position and velocity of Mercury
  mercury_ssb <- position_mercury_ssb(con, t)
  
  # Convert data frames from km and km/sec to AU and AU/day
  earth_ssb_au <- earth_ssb[,1] / KM2AU
  earth_ssb_au_day <- earth_ssb[,2] / KM2AU
  helio_earth_au <- helio_earth[,1] / KM2AU
  helio_earth_au_day <- helio_earth[,2] / KM2AU
  mercury_ssb_au <- mercury_ssb[,1] / KM2AU
  mercury_ssb_au_day <- mercury_ssb[,2] / KM2AU
  
  # Calculate the geometric distance between the positions of the center of mass
  # of Mercury and the Earth
  tmp <- mercury_ssb_au - earth_ssb_au
  d <- magnitude(tmp)
  print(d)

  # Calculate a first approximation to the light-travel time between Mercury
  # and the Earth
  tau = d / CAUD
  tt <- t - tau
  
  # Extract the barycentric position of Mercury and the Sun at t - tau
  mercury_ssb <- position_mercury_ssb(con, tt)
  sun_ssb <- position_sun_ssb(con, tt)
  mercury_ssb_au <- mercury_ssb[,1] / KM2AU
  mercury_ssb_au_day <- mercury_ssb[,2] / KM2AU
  sun_ssb_au <- sun_ssb[,1] / KM2AU
  sun_ssb_au_day <- sun_ssb[,2] / KM2AU
  
  U <- mercury_ssb_au - earth_ssb_au
  Q <- mercury_ssb_au - sun_ssb_au
  U_mag <- magnitude(U)
  Q_mag <- magnitude(Q)
  E_mag <- magnitude(helio_earth_au)
  
  tau_prime <- 0.0
  
  repeat {
    tau_prime <- 
      (U_mag + (MUC * log((E_mag + U_mag + Q_mag) / (E_mag - U_mag + Q_mag)))) / CAUD
    if ((tau - tau_prime) < 1E-8){
      break
    }else{
      # Extract the barycentric position of the body and the Sun at t - tau_prime
      mercury_ssb <- position_mercury_ssb(con, (t - tau_prime))
      sun_ssb <- position_sun_ssb(con, (t - tau_prime))
      mercury_ssb_au <- mercury_ssb[,1] / KM2AU
      mercury_ssb_au_day <- mercury_ssb[,2] / KM2AU
      sun_ssb_au <- sun_ssb[,1] / KM2AU
      sun_ssb_au_day <- sub_ssb[,2] / KM2AU
      
      U <- mercury_ssb_au - earth_ssb_au
      Q <- mecury_ssb_au - sun_ssb_au
      U_mag <- magnitude(U)
      Q_mag <- magnitude(Q)
      E_mag <- magnitude(helio_earth_au)
    }
  }
  
  # Calculate the relativistic deflection of light
  U_uv <- unit_vector(U)
  Q_uv <- unit_vector(Q)
  E_uv <-unit_vector(helio_earth_au)
  g1 <- MUC/E_mag
  g2 <- 1 + dot_product(Q_uv, E_uv)
  dot_UQ <- dot_product(U_uv, Q_uv)
  dot_EU <- dot_product(E_uv, U_uv)
  
  U1 <- U_mag * (U_uv + (g1/g2 * ((dot_UQ*E_uv) - (dot_EU*Q_uv))))

  # Calculate the aberration of light

  # Calculate and apply the precession matrix
  prec <- precession_matrix(t)
  U3 <- prec %*% U1
  
  # Calculate and apply the nutation matrix
  nut <- nutation_matrix(con, t)
  U4 <- nut %*% U3
  
  # Calculate the right ascension and declination
  ra <- atan2(U4[2], U4[1])
  dec <- atan2(U4[3], sqrt(U4[1] * U4[1] + U4[2] * U4[2]))
  ra <- ra * RAD2HR
  dec <- dec * RAD2DEG
  
  print(magnitude(U4))
  
  ecliptic_coords <- c(ra, dec)
  
  return(ecliptic_coords)
}