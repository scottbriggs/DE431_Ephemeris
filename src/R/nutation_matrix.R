
# Create the nutation matrix
nutation_matrix <- function(con, jd)
{
  # Days per Block is 32 for DE431
  days_per_block <- 32
  
  # Calculate the subinterval
  query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Earth Nutations- Longitude and Obliquity, IAU 1980'"
  num_subintervals <- dbGetQuery(con, query)
  length_of_subinterval <- days_per_block / num_subintervals
  
  str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Nutation WHERE Julian_Day_Start <=",
                jd,
                "AND Julian_Day_End >=",
                jd,
                "AND Interval = 1")
  jd_block_start <- dbGetQuery(con, str1)
  length_of_subinterval <- 32 / num_subintervals
  subinterval <- floor((jd - jd_block_start) / length_of_subinterval)
  
  # Add 1 to get the right subinterval. The above algorithm assumes the 
  # subinterval begins with 0, but they begin with 1 in the database
  subinterval <- subinterval + 1
  
  # Get the nutation data
  str2 <- paste("SELECT DISTINCT Longitude1, Longitude2, Longitude3, Longitude4, 
                Longitude5, Longitude6, Longitude7, Longitude8,Longitude9, Longitude10,",
                "Obliquity1, Obliquity2, Obliquity3, Obliquity4, Obliquity5, Obliquity6, 
                Obliquity7, Obliquity8, Obliquity9, Obliquity10",
                "FROM Nutation WHERE",
                jd,
                ">= Julian_Day_Start AND",
                jd,
                "<= Julian_Day_End AND Interval = ",
                subinterval)
  
  nutation_df <- dbGetQuery(con, str2)
  
  # Normalize the Julian Day
  valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
  valid_end <- valid_start + length_of_subinterval
  temp <- jd - valid_start
  x <- (temp / length_of_subinterval * 2.0) - 1.0
  
  # Calculate the Chebyshev polynomial for longitude and obliquity.
  chebyshev <- data.frame(matrix(0.0, nrow=10, ncol=1))
  chebyshev[1,1] <- 1.0
  chebyshev[2,1] <- x
  
  # Calculate the polynomial
  for (i in seq(from = 3, to = 10, by = 1)){
    chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
  }
  
  # Calculate the longitude and obliquity
  vec1 <- data.frame(matrix(0.0, nrow=2, ncol=1))
  v <- 0
  for (v in seq(from = 10, to = 1, by = -1)){
    vec1[1,1] <- vec1[1,1] + (chebyshev[v,1] * nutation_df[v])
    vec1[2,1] <- vec1[2,1] + (chebyshev[v,1] * nutation_df[v+10])
  }
  
  # Calculate the mean and true obliquity of date
  T <- (jd - 2451545.0) / 36525
  T2 <- T * T
  T3 = T2 * T
  term1 <- DDdd(23, 26, 21.448)
  term2 <- DDdd(0, 0, 46.8150) * T
  term3 <- DDdd(0, 0, 0.00059) * T2
  term4 <- DDdd(0, 0, 0.001813) * T3
  mean_obliquity <- deg2rad(term1 + term2 + term3 + term4)
  true_obliquity <- mean_obliquity + vec1[2,1]
  
  # Calculate the nutation matrix elements
  cos_long <- cos(vec1[1,1])
  sin_long <- sin(vec1[1,1])
  sin_mean_obliquity <- sin(mean_obliquity)
  cos_mean_obliquity <- cos(mean_obliquity)
  sin_true_obliquity <- sin(true_obliquity)
  cos_true_obliquity <- cos(true_obliquity)
  
  nut_mat <- matrix(0.0, nrow=3, ncol=3)
  nut_mat[1,1] <- cos_long
  nut_mat[1,2] <- -sin_long * cos_mean_obliquity
  nut_mat[1,3] <- -sin_long * sin_mean_obliquity
  nut_mat[2,1] <- sin_long * cos_true_obliquity
  nut_mat[2,2] <- cos_long * cos_true_obliquity * cos_mean_obliquity + 
    sin_true_obliquity * sin_mean_obliquity
  nut_mat[2,3] <- cos_long * cos_true_obliquity * sin_mean_obliquity -
    sin_true_obliquity * cos_mean_obliquity
  nut_mat[3,1] <- sin_long * sin_true_obliquity
  nut_mat[3,2] <- cos_long * sin_true_obliquity * cos_mean_obliquity -
    cos_true_obliquity * sin_mean_obliquity
  nut_mat[3,3] <- cos_long * sin_true_obliquity * sin_mean_obliquity +
    cos_true_obliquity * cos_mean_obliquity
  
  return(nut_mat)
}