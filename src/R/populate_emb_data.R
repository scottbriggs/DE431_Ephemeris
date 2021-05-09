
# Takes a RDS file of DE431 body data and writes out a csv file of data
# for the Earth-Moon Barycenter (EMB) to be ingested into a database

populate_emb_data <- function(filename, db_conn)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for EMB
  emb_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                       "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                       "X10", "X11", "X12", "X13", "Y1", "Y2", "Y3", "Y4",
                       "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11", "Y12", "Y13",
                       "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8",
                       "Z9", "Z10", "Z11", "Z12", "Z13")
  
  # Create table for EMB
  temp = matrix(0.0,nrow=22830,ncol=42)
  colnames(temp) <- emb_col_names
  emb = as.data.table(temp)
  
  # Populate the intervals for EMB
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    set(emb, i, "INTERVAL", 1)
    set(emb, i+1L, "INTERVAL", 2)
  }
  
  # Populate the Julian Days for EMB
  j <- 1L
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    set(emb, i, "Julian_Day_Start", DT[j, 1L])
    set(emb, i, "Julian_Day_End", DT[j+1L, 1L])
    set(emb, i+1L, "Julian_Day_Start", DT[j, 1L])
    set(emb, i+1L, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinterval 1
  k <- 228L
  for (i in seq(from = 1L, to = 22830L, by = 2L)){
    for (j in seq(from = 3L, to = 41L, by = 1L)){
      set(emb, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 2
  k <- 267L
  for (i in seq(from = 2L, to = 22830L, by = 2L)){
    for (j in seq(from = 3L, to = 41L, by = 1L)){
      set(emb, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for EMB
  #new_filename <- substr(filename, 1, 9)
  #new_filename <- paste(new_filename, "_emb.csv", sep = "")
  #write.csv(emb, here("data", "processed", new_filename), row.names = FALSE)
  
  dbAppendTable(db_conn, "EMB", emb, row.names = NULL)
}