
# Retrieve the nutation angles from the DE431 empeheris

nutation_angles <- function(jd)
{
  # Days per Block is 32 for DE431
  days_per_block <- 32
  
  # Calculate the subinterval
  query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Earth Nutations- Longitude and Obliquity, IAU 1980'"
  num_subintervals <- dbGetQuery(db_con, query)
  length_of_subinterval <- days_per_block / num_subintervals
  
  str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Nutation WHERE Julian_Day_Start <=",
                jd,
                "AND Julian_Day_End >",
                jd,
                "AND Interval = 1")
  jd_block_start <- dbGetQuery(db_con, str1)
  subinterval <- floor(as.integer(jd - jd_block_start) / length_of_subinterval)
  
  # Add 1 to get the right subinterval. The above algorithm assumes the 
  # subinterval begins with 0, but they begin with 1 in the database
  subinterval <- subinterval + 1
  
  # Get the nutation data
  str2 <- paste("SELECT DISTINCT Longitude1, Longitude2, Longitude3, Longitude4, 
                Longitude5, Longitude6, Longitude7, Longitude8,Longitude9, Longitude10,",
                "Obliquity1, Obliquity2, Obliquity3, Obliquity4, Obliquity5, Obliquity6, 
                Obliquity7, Obliquity8, Obliquity9, Obliquity10",
                "FROM Nutation WHERE Julian_Day_Start =",
                jd_block_start,
                "AND Interval = ",
                subinterval)
  
  nutation_df <- dbGetQuery(db_con, str2)
  
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
  vec1 <- c(0.0, 0.0)
  v <- 0
  for (v in seq(from = 10, to = 1, by = -1)){
    vec1[1] <- vec1[1] + (chebyshev[v,1] * nutation_df[v])
    vec1[2] <- vec1[2] + (chebyshev[v,1] * nutation_df[v+10])
  }
  
  return(vec1)
}