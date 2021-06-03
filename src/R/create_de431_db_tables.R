
# Create the tables in sqlite3 for the solar system bodies for the de431
# ephemeris

create_de431_db_tables <- function(db_conn)
{
  # Create table schema for Mercury
  col_bodies_mercury <- c(Julian_Day_Start = "REAL",
                  Julian_Day_End = "REAL",
                  Interval = "INTEGER",
                  X1 = "REAL",
                  X2 = "REAL",
                  X3 = "REAL",
                  X4 = "REAL",
                  X5 = "REAL",
                  X6 = "REAL",
                  X7 = "REAL",
                  X8 = "REAL",
                  X9 = "REAL",
                  X10 = "REAL",
                  X11 = "REAL",
                  X12 = "REAL",
                  X13 = "REAL",
                  X14 = "REAL",
                  Y1 = "REAL",
                  Y2 = "REAL",
                  Y3 = "REAL",
                  Y4 = "REAL",
                  Y5 = "REAL",
                  Y6 = "REAL",
                  Y7 = "REAL",
                  Y8 = "REAL",
                  Y9 = "REAL",
                  Y10 = "REAL",
                  Y11 = "REAL",
                  Y12 = "REAL",
                  Y13 = "REAL",
                  Y14 = "REAL",
                  Z1 = "REAL",
                  Z2 = "REAL",
                  Z3 = "REAL",
                  Z4 = "REAL",
                  Z5 = "REAL",
                  Z6 = "REAL",
                  Z7 = "REAL",
                  Z8 = "REAL",
                  Z9 = "REAL",
                  Z10 = "REAL",
                  Z11 = "REAL",
                  Z12 = "REAL",
                  Z13 = "REAL",
                  Z14 = "REAL")
  
  # Create table schema for Venus
  col_bodies_venus <- c(Julian_Day_Start = "REAL",
                          Julian_Day_End = "REAL",
                          Interval = "INTEGER",
                          X1 = "REAL",
                          X2 = "REAL",
                          X3 = "REAL",
                          X4 = "REAL",
                          X5 = "REAL",
                          X6 = "REAL",
                          X7 = "REAL",
                          X8 = "REAL",
                          X9 = "REAL",
                          X10 = "REAL",
                          Y1 = "REAL",
                          Y2 = "REAL",
                          Y3 = "REAL",
                          Y4 = "REAL",
                          Y5 = "REAL",
                          Y6 = "REAL",
                          Y7 = "REAL",
                          Y8 = "REAL",
                          Y9 = "REAL",
                          Y10 = "REAL",
                          Z1 = "REAL",
                          Z2 = "REAL",
                          Z3 = "REAL",
                          Z4 = "REAL",
                          Z5 = "REAL",
                          Z6 = "REAL",
                          Z7 = "REAL",
                          Z8 = "REAL",
                          Z9 = "REAL",
                          Z10 = "REAL")
  
  # Create table schema for EMB and the Moon
  col_bodies_emb <- c(Julian_Day_Start = "REAL",
                          Julian_Day_End = "REAL",
                          Interval = "INTEGER",
                          X1 = "REAL",
                          X2 = "REAL",
                          X3 = "REAL",
                          X4 = "REAL",
                          X5 = "REAL",
                          X6 = "REAL",
                          X7 = "REAL",
                          X8 = "REAL",
                          X9 = "REAL",
                          X10 = "REAL",
                          X11 = "REAL",
                          X12 = "REAL",
                          X13 = "REAL",
                          Y1 = "REAL",
                          Y2 = "REAL",
                          Y3 = "REAL",
                          Y4 = "REAL",
                          Y5 = "REAL",
                          Y6 = "REAL",
                          Y7 = "REAL",
                          Y8 = "REAL",
                          Y9 = "REAL",
                          Y10 = "REAL",
                          Y11 = "REAL",
                          Y12 = "REAL",
                          Y13 = "REAL",
                          Z1 = "REAL",
                          Z2 = "REAL",
                          Z3 = "REAL",
                          Z4 = "REAL",
                          Z5 = "REAL",
                          Z6 = "REAL",
                          Z7 = "REAL",
                          Z8 = "REAL",
                          Z9 = "REAL",
                          Z10 = "REAL",
                          Z11 = "REAL",
                          Z12 = "REAL",
                          Z13 = "REAL")
  
  # Create table schema for Mars and the Sun
  col_bodies_mars <- c(Julian_Day_Start = "REAL",
                      Julian_Day_End = "REAL",
                      Interval = "INTEGER",
                      X1 = "REAL",
                      X2 = "REAL",
                      X3 = "REAL",
                      X4 = "REAL",
                      X5 = "REAL",
                      X6 = "REAL",
                      X7 = "REAL",
                      X8 = "REAL",
                      X9 = "REAL",
                      X10 = "REAL",
                      X11 = "REAL",
                      Y1 = "REAL",
                      Y2 = "REAL",
                      Y3 = "REAL",
                      Y4 = "REAL",
                      Y5 = "REAL",
                      Y6 = "REAL",
                      Y7 = "REAL",
                      Y8 = "REAL",
                      Y9 = "REAL",
                      Y10 = "REAL",
                      Y11 = "REAL",
                      Z1 = "REAL",
                      Z2 = "REAL",
                      Z3 = "REAL",
                      Z4 = "REAL",
                      Z5 = "REAL",
                      Z6 = "REAL",
                      Z7 = "REAL",
                      Z8 = "REAL",
                      Z9 = "REAL",
                      Z10 = "REAL",
                      Z11 = "REAL")
  
  # Create table schema for Jupiter
  col_bodies_jupiter <- c(Julian_Day_Start = "REAL",
                       Julian_Day_End = "REAL",
                       Interval = "INTEGER",
                       X1 = "REAL",
                       X2 = "REAL",
                       X3 = "REAL",
                       X4 = "REAL",
                       X5 = "REAL",
                       X6 = "REAL",
                       X7 = "REAL",
                       X8 = "REAL",
                       Y1 = "REAL",
                       Y2 = "REAL",
                       Y3 = "REAL",
                       Y4 = "REAL",
                       Y5 = "REAL",
                       Y6 = "REAL",
                       Y7 = "REAL",
                       Y8 = "REAL",
                       Z1 = "REAL",
                       Z2 = "REAL",
                       Z3 = "REAL",
                       Z4 = "REAL",
                       Z5 = "REAL",
                       Z6 = "REAL",
                       Z7 = "REAL",
                       Z8 = "REAL")
  
  # Create table schema for Saturn
  col_bodies_saturn <- c(Julian_Day_Start = "REAL",
                          Julian_Day_End = "REAL",
                          Interval = "INTEGER",
                          X1 = "REAL",
                          X2 = "REAL",
                          X3 = "REAL",
                          X4 = "REAL",
                          X5 = "REAL",
                          X6 = "REAL",
                          X7 = "REAL",
                          Y1 = "REAL",
                          Y2 = "REAL",
                          Y3 = "REAL",
                          Y4 = "REAL",
                          Y5 = "REAL",
                          Y6 = "REAL",
                          Y7 = "REAL",
                          Z1 = "REAL",
                          Z2 = "REAL",
                          Z3 = "REAL",
                          Z4 = "REAL",
                          Z5 = "REAL",
                          Z6 = "REAL",
                          Z7 = "REAL")
  
  # Create table schema for Uranus, Neptune, and Pluto
  col_bodies_uranus <- c(Julian_Day_Start = "REAL",
                         Julian_Day_End = "REAL",
                         Interval = "INTEGER",
                         X1 = "REAL",
                         X2 = "REAL",
                         X3 = "REAL",
                         X4 = "REAL",
                         X5 = "REAL",
                         X6 = "REAL",
                         Y1 = "REAL",
                         Y2 = "REAL",
                         Y3 = "REAL",
                         Y4 = "REAL",
                         Y5 = "REAL",
                         Y6 = "REAL",
                         Z1 = "REAL",
                         Z2 = "REAL",
                         Z3 = "REAL",
                         Z4 = "REAL",
                         Z5 = "REAL",
                         Z6 = "REAL")
  
  # Create table schema for the Nutation data
  col_nutation <- c(Julian_Day_Start = "REAL",
                    Julian_Day_End = "REAL",
                    Interval = "INTEGER",
                    Longitude1 = "REAL",
                    Longitude2 = "REAL",
                    Longitude3 = "REAL",
                    Longitude4 = "REAL",
                    Longitude5 = "REAL",
                    Longitude6 = "REAL",
                    Longitude7 = "REAL",
                    Longitude8 = "REAL",
                    Longitude9 = "REAL",
                    Longitude10 = "REAL",
                    Obliquity1 = "REAL",
                    Obliquity2 = "REAL",
                    Obliquity3 = "REAL",
                    Obliquity4 = "REAL",
                    Obliquity5 = "REAL",
                    Obliquity6 = "REAL",
                    Obliquity7 = "REAL",
                    Obliquity8 = "REAL",
                    Obliquity9 = "REAL",
                    Obliquity10 = "REAL")
  
  # Create table schema for the Body data
  col_body <- c(Body = "TEXT",
                Units = "TEXT",
                Center = "TEXT",
                Properties = "TEXT",
                Start_Offset = "INTEGER",
                Coefficients = "INTEGER",
                Subintervals = "INTEGER")
  
  # Create table schema for the Header data
  col_header <- c(Header_Name = "TEXT",
                  Header_Value = "REAL")
  
  # Create tables for the header and body information
  dbCreateTable(db_conn, "Header", col_header, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "DE431Body", col_body, row.names = NULL,
                temporary = FALSE)
  
  # Create tables for the solar system bodies in database
  dbCreateTable(db_conn, "Mercury", col_bodies_mercury, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Venus", col_bodies_venus, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "EMB", col_bodies_emb, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Mars", col_bodies_mars, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Jupiter", col_bodies_jupiter, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Saturn", col_bodies_saturn, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Uranus", col_bodies_uranus, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Neptune", col_bodies_uranus, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Pluto", col_bodies_uranus, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Sun", col_bodies_mars, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Moon", col_bodies_emb, row.names = NULL,
                temporary = FALSE)
  
  dbCreateTable(db_conn, "Nutation", col_nutation, row.names = NULL,
                temporary = FALSE)
  
}