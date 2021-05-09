
# Calculate the position and velocity of Mercury

calculate_position_mercury <- function(con, jd)
{
   # Days per Block is 32 for DE431
   days_per_block <- 32
   
   # Calculate the subinterval
   query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Mercury'"
   num_subintervals <- dbGetQuery(con, query)
   length_of_subinterval <- days_per_block / num_subintervals
   
   str1 <- paste("SELECT Julian_Day_Start FROM Mercury WHERE Julian_Day_Start <=",
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
   
   # Get the data for Mercury
   str2 <- paste("SELECT X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14,",
                 "Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13, Y14,",
                 "Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11, Z12, Z13, Z14",
                 "FROM Mercury WHERE",
                 jd,
                 ">= Julian_Day_Start AND",
                 jd,
                 "<= Julian_Day_End AND Interval = ",
                 subinterval)
   
   mercury_df <- dbGetQuery(con, str2)
   
   # Normalize the Julian Day
   valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
   valid_end <- valid_start + length_of_subinterval
   temp <- jd - valid_start
   x <- (temp / length_of_subinterval * 2.0) - 1.0
   
   # Calculate the Chebyshev polynomials for position and velocity. The velocity
   # is the first derivative of the position polynomial
   chebyshev <- data.frame(matrix(0.0, nrow=14, ncol=2))
   chebyshev[1,1] <- 1.0
   chebyshev[2,1] <- x
   chebyshev[1,2] <- 0.0
   chebyshev[2,2] <- 1.0
   
   # Calculate the position polynomial
   for (i in seq(from = 3, to = 14, by = 1)){
      chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
   }
   
   # Calculate the velocity polynomial
   for (i in seq(from = 3, to = 14, by = 1)){
      chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
   }
   
   # Calculate the position in kilometers and the velocity in kilometers/sec
   pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
   v <- 0
   for (v in seq(from = 14, to = 1, by = -1)){
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * mercury_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * mercury_df[1,v+14])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * mercury_df[1,v+28])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * mercury_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * mercury_df[1,v+14])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * mercury_df[1,v+28])
   }
   
   # Scale the velocity
   scale_value <- 2 * num_subintervals / 32
   pos_vel[1,2] = pos_vel[1,2] * scale_value
   pos_vel[2,2] = pos_vel[2,2] * scale_value
   pos_vel[3,2] = pos_vel[3,2] * scale_value
   
   # Return the data
   return(pos_vel)
}
