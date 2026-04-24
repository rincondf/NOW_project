males_2012$Date <- as.Date(males_2012$Date, format = "%m/%d/%Y")
males_2012$julian <- as.numeric(format(males_2012$Date, "%j"))


males_2012 <- aggregate(males_2012$Males, list(males_2012$Date, 
                                               males_2012$Ranch, males_2012$Crop), 
                        mean, na.rm = TRUE)

colnames(males_2012) <- c("Date", "loc", "Crop", "moths")



# Matching locs with latitude

males_2012$lat <- rep(NA, length(males_2012$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2012$lat[which(males_2012$loc == (locations_NOW$loc[i]))] <- locations_NOW$latitude[i]
}


# Matching locs with longitude

males_2012$long <- rep(NA, length(males_2012$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2012$long[which(males_2012$loc == (locations_NOW$loc[i]))] <- locations_NOW$longitude[i]
}

males_2012 <- males_2012[which(!is.na(males_2012$lat)), ]



# 1. Formalize dates and calculate julians 

males_2012$julians <- as.numeric(format(males_2012$Date, "%j"))

# 2. Find unique locations and years combinations

locs_NOW2012 <- subset(males_2012, 
                       duplicated(males_2012[c("lat", 
                                               "long")]) == 
                         FALSE)[, c(2, 5, 6)]



# 3. Extract max and min temp per location and year

library(daymetr)

temp_recs <- list()

for(i in 1: length(locs_NOW2012$loc)) {
  temp_recs[[i]] <- download_daymet(
    site = "Daymet",
    lat = locs_NOW2012$lat[i],
    lon = locs_NOW2012$long[i],
    start = 2012,
    end = 2012,
    path = tempdir(),
    internal = TRUE,
    silent = FALSE,
    force = FALSE,
    simplify = FALSE
  )
}


tmax <- list()

for(i in 1: length(locs_NOW2012$loc)) {
  tmax[[i]] <- as.numeric(temp_recs[[i]]$data[,7])
}

tmin <- list()

for(i in 1: length(locs_NOW2012$loc)) {
  tmin[[i]] <- as.numeric(temp_recs[[i]]$data[,8])
}

# 4. Calculate cumulative degree days using max and min temps obtained by Vince

upT <- 12.8 + 19.8
baseT <- 12.8

DDs_NOW <- list()

for(i in 1: length(locs_NOW2012$loc)) {
  DDs_NOW[[i]] <- calc_dd_vec(tmax = tmax[[i]], tmin = tmin[[i]], 
                              lower_threshold = baseT, 
                              upper_threshold = upT, 
                              cutoff = "vertical")
  DDs_NOW[[i]] <- cumsum(DDs_NOW[[i]])
}

# Matching data collection days

males_2012$NOW_DD <- rep(NA, length(males_2012$loc))


for(i in 1: length(locs_NOW2012$loc)) {
  males_2012$NOW_DD[which(males_2012$lat == locs_NOW2012$lat[i] & 
                            males_2012$long == locs_NOW2012$long[i])] <-
    DDs_NOW[[i]][males_2012$julians[which(males_2012$lat == 
                                            locs_NOW2012$lat[i] &
                                            males_2012$long == 
                                            locs_NOW2012$long[i])]]
  
}

plot(males_2012$NOW_DD, males_2012$moths)



males_2012$pmoths <- rep(NA, length(males_2012$loc))

for(i in 1: length(locs_NOW2012$loc)) {
  males_2012$pmoths[which(males_2012$loc == locs_NOW2012$loc[i])] <-
    males_2012$moths[which(males_2012$loc == locs_NOW2012$loc[i])] / 
    sum(males_2012$moths[which(males_2012$loc == locs_NOW2012$loc[i])])
  
}



plot(males_2012$NOW_DD, males_2012$pmoths)


males_2012$cpmoths <- rep(NA, length(males_2012$loc))

for(i in 1: length(locs_NOW2012$loc)) {
  males_2012$cpmoths[which(males_2012$loc == locs_NOW2012$loc[i])] <-
    cumsum(males_2012$moths[which(males_2012$loc == locs_NOW2012$loc[i])] / 
             sum(males_2012$moths[which(males_2012$loc == locs_NOW2012$loc[i])]))
  
}



plot(males_2012$NOW_DD, males_2012$cpmoths)
