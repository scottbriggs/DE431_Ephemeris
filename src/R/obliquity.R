
# Calculate the mean and true obliquity of the ecliptic in radians

obliquity <- function(jd, nut)
{
  # Calculate the mean and true obliquity of date
  T <- (jd - 2451545.0) / 36525
  T2 <- T * T
  T3 = T2 * T
  term1 <- dms2deg(23, 26, 21.448)
  term2 <- dms2deg(0, 0, 46.8150) * T
  term3 <- dms2deg(0, 0, 0.00059) * T2
  term4 <- dms2deg(0, 0, 0.001813) * T3
  mean_obliquity <- (term1 + term2 + term3 + term4) * DEG2RAD
  true_obliquity <- mean_obliquity + nut[[2]]
  
  obliq <- c(mean_obliquity, true_obliquity)
  
  return(obliq)
}