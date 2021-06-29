
# Calculate the mean and true obliquity of the ecliptic in radians

obliquity <- function(jd, nut)
{
  # Calculate the mean and true obliquity of date
  T <- (jd - 2451545.0) / 36525
  T2 <- T * T
  T3 = T2 * T
  term1 <- ddmmss2deg(23, 26, 21.448)
  term2 <- ddmmss2deg(0, 0, 46.8150) * T
  term3 <- ddmmss2deg(0, 0, 0.00059) * T2
  term4 <- ddmmss2deg(0, 0, 0.001813) * T3
  mean_obliquity <- (term1 + term2 + term3 + term4) * DEG2RAD
  true_obliquity <- mean_obliquity + nut[[2]]
  
  obliq <- c(0.0, 0.0)
  obliq[1] <- mean_obliquity
  obliq[2] <- true_obliquity
  
  return(obliq)
}