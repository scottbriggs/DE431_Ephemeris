
# Calculate the SSB position and velocity vectors for the following bodies:
# Mercury, Venus, EMB, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto,
# and the Sun

position_body_ssb <- function(con, jd, body)
{
  if (body == 'Mercury'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Mercury'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Mercury WHERE Julian_Day_Start <=",
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
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14,",
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
    
  }else if (body == 'Venus'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Venus'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Venus WHERE Julian_Day_Start <=",
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
    
    # Get the data for Venus
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6, X7, X8, X9, X10,",
                  "Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10,",
                  "Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10",
                  "FROM Venus WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    venus_df <- dbGetQuery(con, str2)
    
    # Normalize the Julian Day
    valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
    valid_end <- valid_start + length_of_subinterval
    temp <- jd - valid_start
    x <- (temp / length_of_subinterval * 2.0) - 1.0
    
    # Calculate the Chebyshev polynomials for position and velocity. The velocity
    # is the first derivative of the position polynomial
    chebyshev <- data.frame(matrix(0.0, nrow=10, ncol=2))
    chebyshev[1,1] <- 1.0
    chebyshev[2,1] <- x
    chebyshev[1,2] <- 0.0
    chebyshev[2,2] <- 1.0
    
    # Calculate the position polynomial
    for (i in seq(from = 3, to = 10, by = 1)){
      chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
    }
    
    # Calculate the velocity polynomial
    for (i in seq(from = 3, to = 10, by = 1)){
      chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
    }
    
    # Calculate the position in kilometers and the velocity in kilometers/sec
    pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
    v <- 0
    for (v in seq(from = 10, to = 1, by = -1)){
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * venus_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * venus_df[1,v+10])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * venus_df[1,v+20])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * venus_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * venus_df[1,v+10])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * venus_df[1,v+20])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  }else if (body == 'EMB'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Earth-Moon Barycenter'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM EMB WHERE Julian_Day_Start <=",
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
    
    # Get the data for EMB
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13,",
                  "Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11, Y12, Y13,",
                  "Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11, Z12, Z13",
                  "FROM EMB WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    emb_df <- dbGetQuery(con, str2)
    
    # Normalize the Julian Day
    valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
    valid_end <- valid_start + length_of_subinterval
    temp <- jd - valid_start
    x <- (temp / length_of_subinterval * 2.0) - 1.0
    
    # Calculate the Chebyshev polynomials for position and velocity. The velocity
    # is the first derivative of the position polynomial
    chebyshev <- data.frame(matrix(0.0, nrow=13, ncol=2))
    chebyshev[1,1] <- 1.0
    chebyshev[2,1] <- x
    chebyshev[1,2] <- 0.0
    chebyshev[2,2] <- 1.0
    
    # Calculate the position polynomial
    for (i in seq(from = 3, to = 13, by = 1)){
      chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
    }
    
    # Calculate the velocity polynomial
    for (i in seq(from = 3, to = 13, by = 1)){
      chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
    }
    
    # Calculate the position in kilometers and the velocity in kilometers/sec
    pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
    v <- 0
    for (v in seq(from = 13, to = 1, by = -1)){
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * emb_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * emb_df[1,v+13])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * emb_df[1,v+26])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * emb_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * emb_df[1,v+13])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * emb_df[1,v+26])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  }else if (body == 'Mars'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Mars'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Mars WHERE Julian_Day_Start <=",
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
    
    # Get the data for Mars
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11,",
                  "Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11,",
                  "Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11",
                  "FROM Mars WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    mars_df <- dbGetQuery(con, str2)
    
    # Normalize the Julian Day
    valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
    valid_end <- valid_start + length_of_subinterval
    temp <- jd - valid_start
    x <- (temp / length_of_subinterval * 2.0) - 1.0
    
    # Calculate the Chebyshev polynomials for position and velocity. The velocity
    # is the first derivative of the position polynomial
    chebyshev <- data.frame(matrix(0.0, nrow=11, ncol=2))
    chebyshev[1,1] <- 1.0
    chebyshev[2,1] <- x
    chebyshev[1,2] <- 0.0
    chebyshev[2,2] <- 1.0
    
    # Calculate the position polynomial
    for (i in seq(from = 3, to = 11, by = 1)){
      chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
    }
    
    # Calculate the velocity polynomial
    for (i in seq(from = 3, to = 11, by = 1)){
      chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
    }
    
    # Calculate the position in kilometers and the velocity in kilometers/sec
    pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
    v <- 0
    for (v in seq(from = 11, to = 1, by = -1)){
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * mars_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * mars_df[1,v+11])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * mars_df[1,v+22])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * mars_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * mars_df[1,v+11])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * mars_df[1,v+22])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  }else if (body == 'Jupiter'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Jupiter'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Jupiter WHERE Julian_Day_Start <=",
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
    
    # Get the data for Jupiter
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6, X7, X8,",
                  "Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,",
                  "Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8",
                  "FROM Jupiter WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    jupiter_df <- dbGetQuery(con, str2)
    
    # Normalize the Julian Day
    valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
    valid_end <- valid_start + length_of_subinterval
    temp <- jd - valid_start
    x <- (temp / length_of_subinterval * 2.0) - 1.0
    
    # Calculate the Chebyshev polynomials for position and velocity. The velocity
    # is the first derivative of the position polynomial
    chebyshev <- data.frame(matrix(0.0, nrow=8, ncol=2))
    chebyshev[1,1] <- 1.0
    chebyshev[2,1] <- x
    chebyshev[1,2] <- 0.0
    chebyshev[2,2] <- 1.0
    
    # Calculate the position polynomial
    for (i in seq(from = 3, to = 8, by = 1)){
      chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
    }
    
    # Calculate the velocity polynomial
    for (i in seq(from = 3, to = 8, by = 1)){
      chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
    }
    
    # Calculate the position in kilometers and the velocity in kilometers/sec
    pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
    v <- 0
    for (v in seq(from = 8, to = 1, by = -1)){
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * jupiter_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * jupiter_df[1,v+8])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * jupiter_df[1,v+16])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * jupiter_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * jupiter_df[1,v+8])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * jupiter_df[1,v+16])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  }else if (body == 'Saturn'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Saturn'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Saturn WHERE Julian_Day_Start <=",
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
    
    # Get the data for Saturn
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6, X7,",
                  "Y1, Y2, Y3, Y4, Y5, Y6, Y7,",
                  "Z1, Z2, Z3, Z4, Z5, Z6, Z7",
                  "FROM Saturn WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    saturn_df <- dbGetQuery(con, str2)
    
    # Normalize the Julian Day
    valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
    valid_end <- valid_start + length_of_subinterval
    temp <- jd - valid_start
    x <- (temp / length_of_subinterval * 2.0) - 1.0
    
    # Calculate the Chebyshev polynomials for position and velocity. The velocity
    # is the first derivative of the position polynomial
    chebyshev <- data.frame(matrix(0.0, nrow=7, ncol=2))
    chebyshev[1,1] <- 1.0
    chebyshev[2,1] <- x
    chebyshev[1,2] <- 0.0
    chebyshev[2,2] <- 1.0
    
    # Calculate the position polynomial
    for (i in seq(from = 3, to = 7, by = 1)){
      chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
    }
    
    # Calculate the velocity polynomial
    for (i in seq(from = 3, to = 7, by = 1)){
      chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
    }
    
    # Calculate the position in kilometers and the velocity in kilometers/sec
    pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
    v <- 0
    for (v in seq(from = 7, to = 1, by = -1)){
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * saturn_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * saturn_df[1,v+7])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * saturn_df[1,v+14])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * saturn_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * saturn_df[1,v+7])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * saturn_df[1,v+14])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  }else if (body == 'Uranus'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Uranus'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Uranus WHERE Julian_Day_Start <=",
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
    
    # Get the data for Uranus
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6,",
                  "Y1, Y2, Y3, Y4, Y5, Y6,",
                  "Z1, Z2, Z3, Z4, Z5, Z6",
                  "FROM Uranus WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    uranus_df <- dbGetQuery(con, str2)
    
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
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * uranus_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * uranus_df[1,v+6])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * uranus_df[1,v+12])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * uranus_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * uranus_df[1,v+6])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * uranus_df[1,v+12])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  }else if (body == 'Neptune'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Neptune'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Neptune WHERE Julian_Day_Start <=",
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
    
    # Get the data for Neptune
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6,",
                  "Y1, Y2, Y3, Y4, Y5, Y6,",
                  "Z1, Z2, Z3, Z4, Z5, Z6",
                  "FROM Neptune WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    neptune_df <- dbGetQuery(con, str2)
    
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
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * neptune_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * neptune_df[1,v+6])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * neptune_df[1,v+12])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * neptune_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * neptune_df[1,v+6])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * neptune_df[1,v+12])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  }else if (body == 'Pluto'){
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Pluto'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Pluto WHERE Julian_Day_Start <=",
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
    
    # Get the data for Pluto
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6,",
                  "Y1, Y2, Y3, Y4, Y5, Y6,",
                  "Z1, Z2, Z3, Z4, Z5, Z6",
                  "FROM Pluto WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    pluto_df <- dbGetQuery(con, str2)
    
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
    
  }else if (body == 'Sun') {
    
    # Days per Block is 32 for DE431
    days_per_block <- 32
    
    # Calculate the subinterval
    query <- "SELECT Subintervals FROM DE431Body WHERE Body = 'Sun'"
    num_subintervals <- dbGetQuery(con, query)
    length_of_subinterval <- days_per_block / num_subintervals
    
    str1 <- paste("SELECT DISTINCT Julian_Day_Start FROM Sun WHERE Julian_Day_Start <=",
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
    
    # Get the data for the Sun
    str2 <- paste("SELECT DISTINCT X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11,",
                  "Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8, Y9, Y10, Y11,",
                  "Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, Z10, Z11",
                  "FROM Sun WHERE",
                  jd,
                  ">= Julian_Day_Start AND",
                  jd,
                  "<= Julian_Day_End AND Interval = ",
                  subinterval)
    
    sun_df <- dbGetQuery(con, str2)
    
    # Normalize the Julian Day
    valid_start <- jd_block_start + ((subinterval-1) * length_of_subinterval)
    valid_end <- valid_start + length_of_subinterval
    temp <- jd - valid_start
    x <- (temp / length_of_subinterval * 2.0) - 1.0
    
    # Calculate the Chebyshev polynomials for position and velocity. The velocity
    # is the first derivative of the position polynomial
    chebyshev <- data.frame(matrix(0.0, nrow=11, ncol=2))
    chebyshev[1,1] <- 1.0
    chebyshev[2,1] <- x
    chebyshev[1,2] <- 0.0
    chebyshev[2,2] <- 1.0
    
    # Calculate the position polynomial
    for (i in seq(from = 3, to = 11, by = 1)){
      chebyshev[i,1] <- (2 * x * chebyshev[i-1,1]) - chebyshev[i-2,1]
    }
    
    # Calculate the velocity polynomial
    for (i in seq(from = 3, to = 11, by = 1)){
      chebyshev[i,2] <- (2 * x * chebyshev[i-1,2]) - chebyshev[i-2,2] + (2 * chebyshev[i-1,1])
    }
    
    # Calculate the position in kilometers and the velocity in kilometers/sec
    pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
    v <- 0
    for (v in seq(from = 11, to = 1, by = -1)){
      pos_vel[1,1] <- pos_vel[1,1] + (chebyshev[v,1] * sun_df[1,v])
      pos_vel[2,1] <- pos_vel[2,1] + (chebyshev[v,1] * sun_df[1,v+11])
      pos_vel[3,1] <- pos_vel[3,1] + (chebyshev[v,1] * sun_df[1,v+22])
      
      pos_vel[1,2] <- pos_vel[1,2] + (chebyshev[v,2] * sun_df[1,v])
      pos_vel[2,2] <- pos_vel[2,2] + (chebyshev[v,2] * sun_df[1,v+11])
      pos_vel[3,2] <- pos_vel[3,2] + (chebyshev[v,2] * sun_df[1,v+22])
    }
    
    # Scale the velocity
    scale_value <- 2 * num_subintervals / 32
    pos_vel[1,2] = pos_vel[1,2] * scale_value
    pos_vel[2,2] = pos_vel[2,2] * scale_value
    pos_vel[3,2] = pos_vel[3,2] * scale_value
    
    # Return the data
    return(pos_vel)
    
  } else {
    stop(paste("Error: Body = ", body, " not found.", sep = ""))
  }
}