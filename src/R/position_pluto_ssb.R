
position_pluto_ssb <- function(jd)
{
  # Days per Block is 32 for DE431
  days_per_block <- 32
  
  # Calculate the subinterval
  query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Pluto'"
  num_subintervals <- dbGetQuery(db_con, query)
  length_of_subinterval <- days_per_block / num_subintervals
  
  str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Pluto WHERE Julian_Day_Start <=",
                jd,
                "AND Julian_Day_End >",
                jd,
                "AND Interval = 1")
  jd_block_start <- dbGetQuery(db_con, str1)
  subinterval <- floor(as.integer(jd - jd_block_start) / length_of_subinterval)
  
  # Add 1 to get the right subinterval. The above algorithm assumes the 
  # subinterval begins with 0, but they begin with 1 in the database
  subinterval <- subinterval + 1
  
  # Get the data for Pluto
  str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6,",
                "Y1, Y2, Y3, Y4, Y5, Y6,",
                "Z1, Z2, Z3, Z4, Z5, Z6",
                "FROM Pluto WHERE Julian_Day_Start =",
                jd_block_start,
                "AND Interval = ",
                subinterval)
  
  pluto_df <- dbGetQuery(db_con, str2)
  
  # Normalize the Julian Day
  valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
  valid_end <- valid_start + length_of_subinterval
  temp <- jd - valid_start
  x <- (temp / length_of_subinterval * 2.0) - 1.0
  
  # Calculate the Chebyshev polynomials for position and velocity. The velocity
  # is the first derivative of the position polynomial
  chebyshev <- data.frame(matrix(0.0, nrow=6, ncol=2))
  chebyshev[1,1] <- 1.0
  chebyshev[2,1] <- x
  chebyshev[1,2] <- 0.0
  chebyshev[2,2] <- 1.0
  
  # Calculate the position polynomial
  for (i in seq(from = 3, to = 6, by = 1)){
    chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
  }
  
  # Calculate the velocity polynomial
  for (i in seq(from = 3, to = 6, by = 1)){
    chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
  }
  
  # Calculate the position in kilometers and the velocity in kilometers/sec
  pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
  v <- 0
  for (v in seq(from = 6, to = 1, by = -1)){
    pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * pluto_df[1,v])
    pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * pluto_df[1,v+6])
    pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * pluto_df[1,v+12])
    
    pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * pluto_df[1,v])
    pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * pluto_df[1,v+6])
    pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * pluto_df[1,v+12])
  }
  
  # Scale the velocity
  scale_value <- 2 * num_subintervals / 32
  pos_vel[1,2] = pos_vel[1,2] * scale_value
  pos_vel[2,2] = pos_vel[2,2] * scale_value
  pos_vel[3,2] = pos_vel[3,2] * scale_value
  
  # Return the data
  return(pos_vel)
}