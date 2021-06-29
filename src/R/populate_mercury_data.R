
# Takes a RDS file of DE431 body data and writes out a csv file of data
# for Mercury to be ingested into a database

populate_mercury_data <- function(filename)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for Mercury, which has 14 coefficients for X, Y, and Z
  mercury_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                         "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                         "X10", "X11", "X12", "X13", "X14", "Y1", "Y2", "Y3", "Y4",
                         "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11", "Y12", "Y13",
                         "Y14", "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8",
                         "Z9", "Z10", "Z11", "Z12", "Z13", "Z14")
  
  # Create table for Mercury
  temp = matrix(0.0,nrow=45660,ncol=45)
  colnames(temp) <- mercury_col_names
  mercury = as.data.table(temp)
  
  # Populate the intervals for Mercury
  for (i in seq(from = 1L, to = 45660L, by = 4L)){
    set(mercury, i, "INTERVAL", 1)
    set(mercury, i+1L, "INTERVAL", 2)
    set(mercury, i+2L, "INTERVAL", 3)
    set(mercury, i+3L, "INTERVAL", 4)
  }
  
  # Populate the Julian Days for Mercury
  j <- 1L
  for (i in seq(from = 1L, to = 45660L, by = 4L)){
    set(mercury, i, "Julian_Day_Start", DT[j, 1L])
    set(mercury, i, "Julian_Day_End", DT[j+1L, 1L])
    set(mercury, i+1L, "Julian_Day_Start", DT[j, 1L])
    set(mercury, i+1L, "Julian_Day_End", DT[j+1L, 1L])
    set(mercury, i+2L, "Julian_Day_Start", DT[j, 1L])
    set(mercury, i+2L, "Julian_Day_End", DT[j+1L, 1L])
    set(mercury, i+3L, "Julian_Day_Start", DT[j, 1L])
    set(mercury, i+3L, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinterval 1
  k <- 0L
  for (i in seq(from = 1L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(mercury, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 2
  k <- 42L
  for (i in seq(from = 2L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(mercury, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 3
  k <- 84L
  for (i in seq(from = 3L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(mercury, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 4
  k <- 126L
  for (i in seq(from = 4L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(mercury, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for Mercury
  #new_filename <- substr(filename, 1, 9)
  #new_filename <- paste(new_filename, "_mercury.csv", sep = "")
  #write.csv(mercury, here("data", "processed", new_filename), row.names = FALSE)
  
  dbAppendTable(db_con, "Mercury", mercury, row.names = NULL)
}