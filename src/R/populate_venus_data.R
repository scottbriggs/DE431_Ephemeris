
# Takes a RDS file of DE431 body data and writes out a csv file of data
# for Venus to be ingested into a database

populate_venus_data <- function(filename)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for Venus, which is 10 coefficients for X, Y, and Z
  venus_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                       "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                       "X10", "Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8",
                       "Y9", "Y10", "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7",
                       "Z8", "Z9", "Z10")
  
  # Create table for Venus
  temp = matrix(0.0,nrow=22830,ncol=33)
  colnames(temp) <- venus_col_names
  venus = as.data.table(temp)
  
  # Populate the intervals for Venus
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    set(venus, i, "INTERVAL", 1)
    set(venus, i+1L, "INTERVAL", 2)
  }
  
  # Populate the Julian Days for Venus
  j <- 1L
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    set(venus, i, "Julian_Day_Start", DT[j, 1L])
    set(venus, i, "Julian_Day_End", DT[j+1L, 1L])
    set(venus, i+1L, "Julian_Day_Start", DT[j, 1L])
    set(venus, i+1L, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinterval 1
  k <- 168L
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    for (j in seq(from = 3L, to = 32L, by = 1L)){
      set(venus, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 2
  k <- 198L
  for (i in seq(from = 2L, to = 22830L, by = 2L)){
    for (j in seq(from = 3L, to = 32L, by = 1L)){
      set(venus, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for Venus
  #new_filename <- substr(filename, 1, 9)
  #new_filename <- paste(new_filename, "_venus.csv", sep = "")
  #write.csv(venus, here("data", "processed", new_filename), row.names = FALSE)
  
  dbAppendTable(db_con, "Venus", venus, row.names = NULL)
}