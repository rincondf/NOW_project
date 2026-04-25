library(readr)
locations_NOW <- read_csv("data/locationsNOW.csv")
males_2011 <- read_csv("data/males_2011.csv")
males_2011$Males <- as.numeric(males_2011$Males)

males_2011$Date <- as.Date(males_2011$Date, format = "%m/%d/%Y")
males_2011$julian <- as.numeric(format(males_2011$Date, "%j"))


males_2011 <- aggregate(males_2011$Males, list(males_2011$Date, 
                                               males_2011$Ranch, males_2011$Crop), 
                        mean, na.rm = TRUE)

colnames(males_2011) <- c("Date", "loc", "Crop", "moths")



# Matching locs with latitude

males_2011$lat <- rep(NA, length(males_2011$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2011$lat[which(males_2011$loc == (locations_NOW$loc[i]))] <- locations_NOW$latitude[i]
}


# Matching locs with longitude

males_2011$long <- rep(NA, length(males_2011$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2011$long[which(males_2011$loc == (locations_NOW$loc[i]))] <- locations_NOW$longitude[i]
}

males_2011 <- males_2011[which(!is.na(males_2011$lat)), ]



# 1. Formalize dates and calculate julians 

males_2011$julians <- as.numeric(format(males_2011$Date, "%j"))

# 2. Find unique locations and years combinations

locs_NOW2011 <- subset(males_2011, 
                       duplicated(males_2011[c("lat", 
                                               "long")]) == 
                         FALSE)[, c(2, 5, 6)]



# 3. Extract max and min temp per location and year

library(daymetr)

temp_recs <- list()

for(i in 1: length(locs_NOW2011$loc)) {
  temp_recs[[i]] <- download_daymet(
    site = "Daymet",
    lat = locs_NOW2011$lat[i],
    lon = locs_NOW2011$long[i],
    start = 2011,
    end = 2011,
    path = tempdir(),
    internal = TRUE,
    silent = FALSE,
    force = FALSE,
    simplify = FALSE
  )
}


tmax <- list()

for(i in 1: length(locs_NOW2011$loc)) {
  tmax[[i]] <- as.numeric(temp_recs[[i]]$data[,7])
}

tmin <- list()

for(i in 1: length(locs_NOW2011$loc)) {
  tmin[[i]] <- as.numeric(temp_recs[[i]]$data[,8])
}

# 4. Calculate cumulative degree days using max and min temps obtained by Vince

source("aux_functions.R")

upT <- 12.8 + 19.8
baseT <- 12.8

DDs_NOW <- list()

for(i in 1: length(locs_NOW2011$loc)) {
  DDs_NOW[[i]] <- calc_dd_vec(tmax = tmax[[i]], tmin = tmin[[i]], 
                              lower_threshold = baseT, 
                              upper_threshold = upT, 
                              cutoff = "vertical")
  DDs_NOW[[i]] <- cumsum(DDs_NOW[[i]])
}

# Matching data collection days

males_2011$NOW_DD <- rep(NA, length(males_2011$loc))


for(i in 1: length(locs_NOW2011$loc)) {
  males_2011$NOW_DD[which(males_2011$lat == locs_NOW2011$lat[i] & 
                            males_2011$long == locs_NOW2011$long[i])] <-
    DDs_NOW[[i]][males_2011$julians[which(males_2011$lat == 
                                            locs_NOW2011$lat[i] &
                                            males_2011$long == 
                                            locs_NOW2011$long[i])]]
  
}

plot(males_2011$NOW_DD, males_2011$moths)



males_2011$pmoths <- rep(NA, length(males_2011$loc))

for(i in 1: length(locs_NOW2011$loc)) {
  males_2011$pmoths[which(males_2011$loc == locs_NOW2011$loc[i])] <-
    males_2011$moths[which(males_2011$loc == locs_NOW2011$loc[i])] / 
    sum(males_2011$moths[which(males_2011$loc == locs_NOW2011$loc[i])])
  
}



plot(males_2011$NOW_DD, males_2011$pmoths)


males_2011$cpmoths <- rep(NA, length(males_2011$loc))

for(i in 1: length(locs_NOW2011$loc)) {
  males_2011$cpmoths[which(males_2011$loc == locs_NOW2011$loc[i])] <-
    cumsum(males_2011$moths[which(males_2011$loc == locs_NOW2011$loc[i])] / 
             sum(males_2011$moths[which(males_2011$loc == locs_NOW2011$loc[i])]))
  
}



plot(males_2011$NOW_DD, males_2011$cpmoths)


save(males_2011, file = "males2011.RData")
