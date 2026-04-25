library(readr)
locations_NOW <- read_csv("data/locationsNOW.csv")
males_2013 <- read_csv("data/males_2013.csv")

males_2013$Date <- as.Date(males_2013$Date, format = "%m/%d/%Y")
males_2013$julian <- as.numeric(format(males_2013$Date, "%j"))


males_2013 <- aggregate(males_2013$Males, list(males_2013$Date, 
                                               males_2013$Ranch, males_2013$Crop), 
                        mean, na.rm = TRUE)

colnames(males_2013) <- c("Date", "loc", "Crop", "moths")



# Matching locs with latitude

males_2013$lat <- rep(NA, length(males_2013$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2013$lat[which(males_2013$loc == (locations_NOW$loc[i]))] <- locations_NOW$latitude[i]
}


# Matching locs with longitude

males_2013$long <- rep(NA, length(males_2013$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2013$long[which(males_2013$loc == (locations_NOW$loc[i]))] <- locations_NOW$longitude[i]
}

males_2013 <- males_2013[which(!is.na(males_2013$lat)), ]



# 1. Formalize dates and calculate julians 

males_2013$julians <- as.numeric(format(males_2013$Date, "%j"))

# 2. Find unique locations and years combinations

locs_NOW2013 <- subset(males_2013, 
                       duplicated(males_2013[c("lat", 
                                               "long")]) == 
                         FALSE)[, c(2, 5, 6)]



# 3. Extract max and min temp per location and year

library(daymetr)

temp_recs <- list()

for(i in 1: length(locs_NOW2013$loc)) {
  temp_recs[[i]] <- download_daymet(
    site = "Daymet",
    lat = locs_NOW2013$lat[i],
    lon = locs_NOW2013$long[i],
    start = 2013,
    end = 2013,
    path = tempdir(),
    internal = TRUE,
    silent = FALSE,
    force = FALSE,
    simplify = FALSE
  )
}


tmax <- list()

for(i in 1: length(locs_NOW2013$loc)) {
  tmax[[i]] <- as.numeric(temp_recs[[i]]$data[,7])
}

tmin <- list()

for(i in 1: length(locs_NOW2013$loc)) {
  tmin[[i]] <- as.numeric(temp_recs[[i]]$data[,8])
}

# 4. Calculate cumulative degree days using max and min temps obtained by Vince

source("aux_functions.R")

upT <- 12.8 + 19.8
baseT <- 12.8

DDs_NOW <- list()

for(i in 1: length(locs_NOW2013$loc)) {
  DDs_NOW[[i]] <- calc_dd_vec(tmax = tmax[[i]], tmin = tmin[[i]], 
                              lower_threshold = baseT, 
                              upper_threshold = upT, 
                              cutoff = "vertical")
  DDs_NOW[[i]] <- cumsum(DDs_NOW[[i]])
}

# Matching data collection days

males_2013$NOW_DD <- rep(NA, length(males_2013$loc))


for(i in 1: length(locs_NOW2013$loc)) {
  males_2013$NOW_DD[which(males_2013$lat == locs_NOW2013$lat[i] & 
                            males_2013$long == locs_NOW2013$long[i])] <-
    DDs_NOW[[i]][males_2013$julians[which(males_2013$lat == 
                                            locs_NOW2013$lat[i] &
                                            males_2013$long == 
                                            locs_NOW2013$long[i])]]
  
}

plot(males_2013$NOW_DD, males_2013$moths)



males_2013$pmoths <- rep(NA, length(males_2013$loc))

for(i in 1: length(locs_NOW2013$loc)) {
  males_2013$pmoths[which(males_2013$loc == locs_NOW2013$loc[i])] <-
    males_2013$moths[which(males_2013$loc == locs_NOW2013$loc[i])] / 
    sum(males_2013$moths[which(males_2013$loc == locs_NOW2013$loc[i])])
  
}



plot(males_2013$NOW_DD, males_2013$pmoths)


males_2013$cpmoths <- rep(NA, length(males_2013$loc))

for(i in 1: length(locs_NOW2013$loc)) {
  males_2013$cpmoths[which(males_2013$loc == locs_NOW2013$loc[i])] <-
    cumsum(males_2013$moths[which(males_2013$loc == locs_NOW2013$loc[i])] / 
             sum(males_2013$moths[which(males_2013$loc == locs_NOW2013$loc[i])]))
  
}



plot(males_2013$NOW_DD, males_2013$cpmoths)

save(males_2013, file = "males2013.RData")
