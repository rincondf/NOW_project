library(readr)
locations_NOW <- read_csv("data/locationsNOW.csv")
males_2010 <- read_csv("data/males_2010.csv")

males_2010$Date <- as.Date(males_2010$Date, format = "%m/%d/%Y")
males_2010$julian <- as.numeric(format(males_2010$Date, "%j"))


males_2010 <- aggregate(males_2010$Males, list(males_2010$Date, 
                                               males_2010$Ranch, males_2010$Crop), 
                        mean, na.rm = TRUE)

colnames(males_2010) <- c("Date", "loc", "Crop", "moths")



# Matching locs with latitude

males_2010$lat <- rep(NA, length(males_2010$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2010$lat[which(males_2010$loc == (locations_NOW$loc[i]))] <- locations_NOW$latitude[i]
}


# Matching locs with longitude

males_2010$long <- rep(NA, length(males_2010$loc))


for(i in 1: length(locations_NOW$loc)) {
  males_2010$long[which(males_2010$loc == (locations_NOW$loc[i]))] <- locations_NOW$longitude[i]
}

males_2010 <- males_2010[which(!is.na(males_2010$lat)), ]



# 1. Formalize dates and calculate julians 

males_2010$julians <- as.numeric(format(males_2010$Date, "%j"))

# 2. Find unique locations and years combinations

locs_NOW2010 <- subset(males_2010, 
                       duplicated(males_2010[c("lat", 
                                               "long")]) == 
                         FALSE)[, c(2, 5, 6)]



# 3. Extract max and min temp per location and year

library(daymetr)

temp_recs <- list()

for(i in 1: length(locs_NOW2010$loc)) {
  temp_recs[[i]] <- download_daymet(
    site = "Daymet",
    lat = locs_NOW2010$lat[i],
    lon = locs_NOW2010$long[i],
    start = 2010,
    end = 2010,
    path = tempdir(),
    internal = TRUE,
    silent = FALSE,
    force = FALSE,
    simplify = FALSE
  )
}


tmax <- list()

for(i in 1: length(locs_NOW2010$loc)) {
  tmax[[i]] <- as.numeric(temp_recs[[i]]$data[,7])
}

tmin <- list()

for(i in 1: length(locs_NOW2010$loc)) {
  tmin[[i]] <- as.numeric(temp_recs[[i]]$data[,8])
}

# 4. Calculate cumulative degree days using max and min temps obtained by Vince

source("aux_functions.R")

upT <- 12.8 + 19.8
baseT <- 12.8

DDs_NOW <- list()

for(i in 1: length(locs_NOW2010$loc)) {
  DDs_NOW[[i]] <- calc_dd_vec(tmax = tmax[[i]], tmin = tmin[[i]], 
                              lower_threshold = baseT, 
                              upper_threshold = upT, 
                              cutoff = "vertical")
  DDs_NOW[[i]] <- cumsum(DDs_NOW[[i]])
}

# Matching data collection days

males_2010$NOW_DD <- rep(NA, length(males_2010$loc))


for(i in 1: length(locs_NOW2010$loc)) {
  males_2010$NOW_DD[which(males_2010$lat == locs_NOW2010$lat[i] & 
                            males_2010$long == locs_NOW2010$long[i])] <-
    DDs_NOW[[i]][males_2010$julians[which(males_2010$lat == 
                                            locs_NOW2010$lat[i] &
                                            males_2010$long == 
                                            locs_NOW2010$long[i])]]

}

plot(males_2010$NOW_DD, males_2010$moths)



males_2010$pmoths <- rep(NA, length(males_2010$loc))

for(i in 1: length(locs_NOW2010$loc)) {
  males_2010$pmoths[which(males_2010$loc == locs_NOW2010$loc[i])] <-
    males_2010$moths[which(males_2010$loc == locs_NOW2010$loc[i])] / 
    sum(males_2010$moths[which(males_2010$loc == locs_NOW2010$loc[i])])
  
}



plot(males_2010$NOW_DD, males_2010$pmoths)


males_2010$cpmoths <- rep(NA, length(males_2010$loc))

for(i in 1: length(locs_NOW2010$loc)) {
  males_2010$cpmoths[which(males_2010$loc == locs_NOW2010$loc[i])] <-
    cumsum(males_2010$moths[which(males_2010$loc == locs_NOW2010$loc[i])] / 
             sum(males_2010$moths[which(males_2010$loc == locs_NOW2010$loc[i])]))
  
}



plot(males_2010$NOW_DD, males_2010$cpmoths)








      