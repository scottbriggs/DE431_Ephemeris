
# Takes a RDS file of DE431 body data and writes out a csv file of data
# for Pluto to be ingested into a database

populate_pluto_data <- function(filename, db_conn)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for Pluto
  pluto_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                      "X1", "X2", "X3", "X4", "X5", "X6",
                      "Y1", "Y2", "Y3", "Y4", "Y5", "Y6", 
                      "Z1", "Z2", "Z3", "Z4", "Z5", "Z6")
  
  # Create table for Pluto
  temp = matrix(0.0,nrow=11415,ncol=21)
  colnames(temp) <- pluto_col_names
  pluto = as.data.table(temp)
  
  # Populate the intervals for Pluto
  for (i in seq(from = 1L, to = 11415L, by = 1L)){
    set(pluto, i, "INTERVAL", 1)
  }
  
  # Populate the Julian Days for Pluto
  j <- 1L
  for (i in seq(from = 1L, to = 11415L, by = 1L)){
    set(pluto, i, "Julian_Day_Start", DT[j, 1L])
    set(pluto, i, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinterval 1
  k <- 420L
  for (i in seq(from = 1L, to = 11415L, by = 1L)){
    for (j in seq(from = 3L, to = 20L, by = 1L)){
      set(pluto, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for Venus
  #new_filename <- substr(filename, 1, 9)
  #new_filename <- paste(new_filename, "_pluto.csv", sep = "")
  #write.csv(pluto, here("data", "processed", new_filename), row.names = FALSE)
  
  dbAppendTable(db_conn, "Pluto", pluto, row.names = NULL)
}