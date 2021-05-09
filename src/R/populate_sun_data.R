
# Takes a RDS file of DE431 body data and writes out a csv file of data
# for the Sun to be ingested into a database

populate_sun_data <- function(filename, db_conn)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for the Sun
  sun_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                       "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                       "X10", "X11", "Y1", "Y2", "Y3", "Y4",
                       "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11",
                       "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8",
                       "Z9", "Z10", "Z11")
  
  # Create table for the Sun
  temp = matrix(0.0,nrow=22830,ncol=36)
  colnames(temp) <- sun_col_names
  sun = as.data.table(temp)
  
  # Populate the intervals for the Sun
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    set(sun, i, "INTERVAL", 1)
    set(sun, i+1L, "INTERVAL", 2)
  }
  
  # Populate the Julian Days for the Sun
  j <- 1L
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    set(sun, i, "Julian_Day_Start", DT[j, 1L])
    set(sun, i, "Julian_Day_End", DT[j+1L, 1L])
    set(sun, i+1L, "Julian_Day_Start", DT[j, 1L])
    set(sun, i+1L, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinterval 1
  k <- 750L
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    for (j in seq(from = 3L, to = 35L, by = 1L)){
      set(sun, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 2
  k <- 783L
  for (i in seq(from = 2L, to = 22830L, by = 2L)){
    for (j in seq(from = 3L, to = 35L, by = 1L)){
      set(sun, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for the Sun
  #new_filename <- substr(filename, 1, 9)
  #new_filename <- paste(new_filename, "_sun.csv", sep = "")
  #write.csv(sun, here("data", "processed", new_filename), row.names = FALSE)
  
  dbAppendTable(db_conn, "Sun", sun, row.names = NULL)
}