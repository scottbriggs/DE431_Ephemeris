
# Takes a RDS file of DE431 body data and writes out a csv file of data
# for the Moon to be ingested into a database

create_moon_csv <- function(filename)
{
  # Read ascii data
  DT <- readRDS(here("data", "processed", filename))
  
  # Column names for the Moon
  moon_col_names <- c("Julian_Day_Start", "Julian_Day_End",
                         "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                         "X10", "X11", "X12", "X13", "X14", "Y1", "Y2", "Y3", "Y4",
                         "Y5", "Y6", "Y7", "Y8", "Y9", "Y10", "Y11", "Y12", "Y13",
                         "Y14", "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8",
                         "Z9", "Z10", "Z11", "Z12", "Z13", "Z14", "INTERVAL")
  
  # Create table for Moon
  temp = matrix(0.0,nrow=91320,ncol=45)
  colnames(temp) <- moon_col_names
  moon = as.data.table(temp)
  
  # Populate the intervals for the Moon
  for (i in seq(from = 1L, to = 91320L, by = 8L)){
    set(moon, i, "INTERVAL", 1)
    set(moon, i+1L, "INTERVAL", 2)
    set(moon, i+2L, "INTERVAL", 3)
    set(moon, i+3L, "INTERVAL", 4)
    set(moon, i+4L, "INTERVAL", 5)
    set(moon, i+5L, "INTERVAL", 6)
    set(moon, i+6L, "INTERVAL", 7)
    set(moon, i+7L, "INTERVAL", 8)
  }
  
  # Populate the Julian Days for the Moon
  j <- 1L
  for (i in seq(from = 1L, to = 91320L, by = 8L)){
    set(moon, i, "Julian_Day_Start", DT[j, 1L])
    set(moon, i, "Julian_Day_End", DT[j+1L, 1L])
    set(moon, i+1L, "Julian_Day_Start", DT[j, 1L])
    set(moon, i+1L, "Julian_Day_End", DT[j+1L, 1L])
    set(moon, i+2L, "Julian_Day_Start", DT[j, 1L])
    set(moon, i+2L, "Julian_Day_End", DT[j+1L, 1L])
    set(moon, i+3L, "Julian_Day_Start", DT[j, 1L])
    set(moon, i+3L, "Julian_Day_End", DT[j+1L, 1L])
    set(moon, i+4L, "Julian_Day_Start", DT[j, 1L])
    set(moon, i+4L, "Julian_Day_End", DT[j+1L, 1L])
    set(moon, i+5L, "Julian_Day_Start", DT[j, 1L])
    set(moon, i+5L, "Julian_Day_End", DT[j+1L, 1L])
    set(moon, i+6L, "Julian_Day_Start", DT[j, 1L])
    set(moon, i+6L, "Julian_Day_End", DT[j+1L, 1L])
    set(moon, i+7L, "Julian_Day_Start", DT[j, 1L])
    set(moon, i+7L, "Julian_Day_End", DT[j+1L, 1L])
    j <- j + 1020L
  }
  
  # Populate subinternval 1
  k <- 438L
  for (i in seq(from = 1L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinternval 2
  k <- 477L
  for (i in seq(from = 2L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinternval 3
  k <- 516L
  for (i in seq(from = 3L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinternval 4
  k <- 555L
  for (i in seq(from = 4L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinternval 5
  k <- 594L
  for (i in seq(from = 5L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinternval 6
  k <- 633L
  for (i in seq(from = 6L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinternval 7
  k <- 672L
  for (i in seq(from = 7L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Populate subinternval 8
  k <- 711L
  for (i in seq(from = 8L, to = 91320L, by = 8L)){
    for (j in seq(from = 3L, to = 44L, by = 1L)){
      set(moon, i, j, DT[j+k, 1L])
    }
    k <- k + 1020L
  }
  
  # Write csv file for the Moon
  new_filename <- substr(filename, 1, 9)
  new_filename <- paste(new_filename, "_moon.csv", sep = "")
  write.csv(moon, here("data", "processed", new_filename), row.names = FALSE)
}