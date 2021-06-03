
# Takes a RDS file of DE431 body data and inserts the nutation data into a
# sqlite database

populate_nutation_data <- function(filename, db_conn)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for Nutations, which has 10 coefficients for longitude and
  # obliquity
  nutation_col_names <- c("Julian_Day_Start", "Julian_Day_End", "INTERVAL",
                         "Longitude1", "Longitude2", "Longitude3", "Longitude4",
                         "Longitude5", "Longitude6", "Longitude7", "Longitude8",
                         "Longitude9", "Longitude10", "Obliquity1", "Obliquity2",
                         "Obliquity3", "Obliquity4","Obliquity5", "Obliquity6",
                         "Obliquity7", "Obliquity8", "Obliquity9", "Obliquity10")
  
  # Create table for Nutations
  temp = matrix(0.0,nrow=45660,ncol=23)
  colnames(temp) <- nutation_col_names
  nutation = as.data.table(temp)
  
  # Populate the intervals for Nutation
  for (i in seq(from = 1L, to = 45660L, by = 4L)){
    set(nutation, i, "INTERVAL", 1)
    set(nutation, i+1L, "INTERVAL", 2)
    set(nutation, i+2L, "INTERVAL", 3)
    set(nutation, i+3L, "INTERVAL", 4)
  }
  
  # Populate the Julian Days for Nutation
  j <- 1L
  for (i in seq(from = 1L, to = 45660L, by = 4L)){
    set(nutation, i, "Julian_Day_Start", DT[j, 1L])
    set(nutation, i, "Julian_Day_End", DT[j+1L, 1L])
    set(nutation, i+1L, "Julian_Day_Start", DT[j, 1L])
    set(nutation, i+1L, "Julian_Day_End", DT[j+1L, 1L])
    set(nutation, i+2L, "Julian_Day_Start", DT[j, 1L])
    set(nutation, i+2L, "Julian_Day_End", DT[j+1L, 1L])
    set(nutation, i+3L, "Julian_Day_Start", DT[j, 1L])
    set(nutation, i+3L, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinterval 1
  k <- 816L
  for (i in seq(from = 1L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 22L, by = 1L)){
      set(nutation, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 2
  k <- 836L
  for (i in seq(from = 2L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 22L, by = 1L)){
      set(nutation, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 3
  k <- 856L
  for (i in seq(from = 3L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 22L, by = 1L)){
      set(nutation, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinterval 4
  k <- 876L
  for (i in seq(from = 4L, to = 45660L, by = 4L)){
    for (j in seq(from = 3L, to = 22L, by = 1L)){
      set(nutation, i, j+1L, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for Nutation
  #new_filename <- substr(filename, 1, 9)
  #new_filename <- paste(new_filename, "_nutation.csv", sep = "")
  #write.csv(nutation, here("data", "processed", new_filename), row.names = FALSE)
  
  dbAppendTable(db_conn, "Nutation", nutation, row.names = NULL)
}