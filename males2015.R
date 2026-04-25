library(readr)
locations_NOW <- read_csv("data/locationsNOW.csv")
males_2015 <- read_csv("data/males_2015.csv")

males_2015$Date <- as.Date(males_2015$Date, format = "%m/%d/%Y")
males_2015$julian <- as.numeric(format(males_2015$Date, "%j"))


males_2015 <- aggregate(males_2015$Males, list(males_2015$Date, 
                                               males_2015$Ranch, males_2015$Crop), 
                        mean, na.rm = TRUE)

colnames(males_2015) <- c("Date", "loc", "Crop", "moths")



# Matching locs with latitude

males_2015$lat <- rep(NA, length(males_2015$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2015$lat[which(males_2015$loc == (locations_NOW$loc[i]))] <- locations_NOW$latitude[i]
}


# Matching locs with longitude

males_2015$long <- rep(NA, length(males_2015$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2015$long[which(males_2015$loc == (locations_NOW$loc[i]))] <- locations_NOW$longitude[i]
}

males_2015 <- males_2015[which(!is.na(males_2015$lat)), ]



# 1. Formalize dates and calculate julians 

males_2015$julians <- as.numeric(format(males_2015$Date, "%j"))

# 2. Find unique locations and years combinations

locs_NOW2015 <- subset(males_2015, 
                       duplicated(males_2015[c("lat", 
                                               "long")]) == 
                         FALSE)[, c(2, 5, 6)]



# 3. Extract max and min temp per location and year

library(daymetr)

temp_recs <- list()

for(i in 1: length(locs_NOW2015$loc)) {
  temp_recs[[i]] <- download_daymet(
    site = "Daymet",
    lat = locs_NOW2015$lat[i],
    lon = locs_NOW2015$long[i],
    start = 2015,
    end = 2015,
    path = tempdir(),
    internal = TRUE,
    silent = FALSE,
    force = FALSE,
    simplify = FALSE
  )
}


tmax <- list()

for(i in 1: length(locs_NOW2015$loc)) {
  tmax[[i]] <- as.numeric(temp_recs[[i]]$data[,7])
}

tmin <- list()

for(i in 1: length(locs_NOW2015$loc)) {
  tmin[[i]] <- as.numeric(temp_recs[[i]]$data[,8])
}

# 4. Calculate cumulative degree days using max and min temps obtained by Vince

source("aux_functions.R")

upT <- 12.8 + 19.8
baseT <- 12.8

DDs_NOW <- list()

for(i in 1: length(locs_NOW2015$loc)) {
  DDs_NOW[[i]] <- calc_dd_vec(tmax = tmax[[i]], tmin = tmin[[i]], 
                              lower_threshold = baseT, 
                              upper_threshold = upT, 
                              cutoff = "vertical")
  DDs_NOW[[i]] <- cumsum(DDs_NOW[[i]])
}

# Matching data collection days

males_2015$NOW_DD <- rep(NA, length(males_2015$loc))


for(i in 1: length(locs_NOW2015$loc)) {
  males_2015$NOW_DD[which(males_2015$lat == locs_NOW2015$lat[i] & 
                            males_2015$long == locs_NOW2015$long[i])] <-
    DDs_NOW[[i]][males_2015$julians[which(males_2015$lat == 
                                            locs_NOW2015$lat[i] &
                                            males_2015$long == 
                                            locs_NOW2015$long[i])]]
  
}

plot(males_2015$NOW_DD, males_2015$moths)



males_2015$pmoths <- rep(NA, length(males_2015$loc))

for(i in 1: length(locs_NOW2015$loc)) {
  males_2015$pmoths[which(males_2015$loc == locs_NOW2015$loc[i])] <-
    males_2015$moths[which(males_2015$loc == locs_NOW2015$loc[i])] / 
    sum(males_2015$moths[which(males_2015$loc == locs_NOW2015$loc[i])])
  
}



plot(males_2015$NOW_DD, males_2015$pmoths)


males_2015$cpmoths <- rep(NA, length(males_2015$loc))

for(i in 1: length(locs_NOW2015$loc)) {
  males_2015$cpmoths[which(males_2015$loc == locs_NOW2015$loc[i])] <-
    cumsum(males_2015$moths[which(males_2015$loc == locs_NOW2015$loc[i])] / 
             sum(males_2015$moths[which(males_2015$loc == locs_NOW2015$loc[i])]))
  
}



plot(males_2015$NOW_DD, males_2015$cpmoths)

save(males_2015, file = "males2015.RData")
