
# Takes a RDS file of DE431 body data and writes out a csv file of data
# for Saturn to be ingested into a database

populate_saturn_data <- function(filename, db_conn)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for Saturn
  saturn_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                      "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                      "X10", "X11", "X12", "X13", "X14", "Y1", "Y2", "Y3", "Y4",
                      "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11", "Y12", "Y13",
                      "Y14", "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8",
                      "Z9", "Z10", "Z11", "Z12", "Z13", "Z14")
  
  # Create table for Saturn
  temp = matrix(0.0,nrow=11415,ncol=45)
  colnames(temp) <- saturn_col_names
  saturn = as.data.table(temp)
  
  # Populate the intervals for Saturn
  for (i in seq(from = 1L, to = 11415L, by = 1L)){
    set(saturn, i, "INTERVAL", 1)
  }
  
  # Populate the Julian Days for Saturn
  j <- 1L
  for (i in seq(from = 1L, to = 11415L, by = 1L)){
    set(saturn, i, "Julian_Day_Start", DT[j, 1L])
    set(saturn, i, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinterval 1
  k <- 363L
  for (i in seq(from = 1L, to = 11415L, by = 1L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(saturn, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for Venus
  #new_filename <- substr(filename, 1, 9)
  #new_filename <- paste(new_filename, "_saturn.csv", sep = "")
  #write.csv(saturn, here("data", "processed", new_filename), row.names = FALSE)
  
  dbAppendTable(db_conn, "Saturn", saturn, row.names = NULL)
}