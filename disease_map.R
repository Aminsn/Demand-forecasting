library(SpatialEpiApp)
library(rgdal) 
library(SpatialEpi)
library(tidyr)
library(sf)

namecsv <- "SpatialEpiApp/data/Ohio/dataohiocomplete.csv"

dohio <- read.csv(system.file(namecsv, package = "SpatialEpiApp"))

head(dohio)

nameshp <- system.file( "SpatialEpiApp/data/Ohio/fe_2007_39_county/fe_2007_39_county.shp", package = "SpatialEpiApp")
map <- readOGR(nameshp, verbose = FALSE)


d <- aggregate(
  x = dohio$y,
  by = list(county = dohio$NAME, year = dohio$year), FUN = sum
)

names(d) <- c("county", "year", "Y")

dohio <- dohio[order( dohio$county, dohio$year, dohio$gender, dohio$race
), ]

n.strata <- 4

E <- expected(
  population = dohio$n, cases = dohio$y, n.strata = n.strata
)


nyears <- length(unique(dohio$year)) 

countiesE <- rep(unique(dohio$NAME), each = nyears)

ncounties <- length(unique(dohio$NAME)) 

yearsE <- rep(unique(dohio$year), times = ncounties)

dE <- data.frame(county = countiesE, year = yearsE, E = E)

d <- merge(d, dE, by = c("county", "year"))

d$SIR <- d$Y / d$E

#Adding data to the map

dw <- reshape(d, timevar = "year", idvar = "county", direction = "wide"
)

map <- merge(map, dw, by.x = "NAME", by.y = "county")

map_sf <- st_as_sf(map)

map_sf <- gather(map_sf, year, SIR, paste0("SIR.", 1968:1988))

map_sf$year <- as.integer(substring(map_sf$year, 5, 8))


