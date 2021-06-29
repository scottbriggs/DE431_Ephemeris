
# Calculate sidereal time
sidereal_time <- function(jd)
{
  # Number of centuries since J2000
  Tu <- (jd - EPOCHJ2000) / DAYSJULCENT
  
  Tu2 <- Tu * Tu
  Tu3 <- Tu2 * Tu
  
  # Greenwich mean standard time
  theta_mean <- 67310.54841 + (876600 * 3600.0 + 8640184.812866) * Tu +
    0.093104 * Tu2 - 6.2E-6 * Tu3
  
  # Convert to radians
  theta_mean <- amodulo(theta_mean / 3600.0, 24) * HR2RAD

  # Get the nutation angles
  nut <- nutation_angles(jd)
  
  # Get obliquity
  obliq <- obliquity(jd, nut)
  
  # Greenwich apparent sidereal time
  theta <- theta_mean + nut[[1]] * cos(obliq[[2]])
  
  gst <- c(0.0, 0.0)
  gst[1] <- theta_mean * RAD2HR
  gst[2] <- theta * RAD2HR
  
  return(gst)
}