
# Ephemeris Constants

# PI
PI <- 3.14159265358979324

# Two PI
PI2 <- 2 * PI

# Conversion to radians from degrees
DEG2RAD <- PI / 180.0

# Conversion to degrees from radians
RAD2DEG <- 180.0 / PI

# Conversion to radians from hours
HR2RAD <- PI / 12.0

# Conversion to hours from radians
RAD2HR <- 12.0 / PI

# Astronomical Unit in Kilometers (KM)
KM2AU <- 149597870.7

# Astronomical Unit in Meters
M2AU <- 1.49597870E11

# Speed of Light in KM per sec
CLIGHT <- 299792.458

# Speed of Light in AU / day
CAUD <- 173.144633

# Arcseconds to Radians
AS2R <- 4.848136811095359935899141E-6

# Gaussian Gravitational Constant
GAUSSK <- 0.01720209895

# EMRAT
EMRAT <- 81.3005690741906

# Seconds per Day
SEC2DAY <- 86400

# MUC
MUC <- 2 * GAUSSK * GAUSSK / CAUD * CAUD

# Equatorial Radius of the Earth in meters
EARTHRADM <- 6378140.0
EARTHRADAU <- EARTHRADM / 1000 / KM2AU

# Flattening of the Earth's reference ellipsoid (IAU 1976)
FLAT <- 1/298.257

# Julian day for J2000
EPOCHJ2000 <- 2451545.0

# Days per julian century
DAYSJULCENT <- 36525

# Rotational angular velocity of the Earth in radians/second
ROTANGVELEARTH <- 7.29211511467e-5
