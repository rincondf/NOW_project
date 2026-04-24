# DEGREE DAY CALCULATION AND PHENOLOGY MODELING OF NAVEL ORANGE WORM

# PRERAPRED BY DIEGO RINCON (diego.rincon@wsu.edu)

# 1. Formalize dates and calculate julians 

data_clean$date <- as.Date(data_clean$date, format = "%m/%d/%Y")
data_clean$julian <- as.numeric(format(data_clean$date, "%j"))

# 2. Find unique locations and years combinations

locations_NOW <- subset(data_clean, 
                        duplicated(data_clean[c("latitude", 
                                                "longitude", 
                                                "year")]) == 
                          FALSE)[, c(1, 2, 3, 4)]

write.csv(locations_NOW, file = "locationsNOW.csv")



# 3. Extract max and min temp per location and year

library(daymetr)

temp_recs <- list()

for(i in 1: length(locations_NOW$loc)) {
  temp_recs[[i]] <- download_daymet(
    site = "Daymet",
    lat = locations_NOW$latitude[i],
    lon = locations_NOW$longitude[i],
    start = locations_NOW$year[i],
    end = locations_NOW$year[i],
    path = tempdir(),
    internal = TRUE,
    silent = FALSE,
    force = FALSE,
    simplify = FALSE
  )
}


tmax <- list()

for(i in 1: length(locations_NOW$loc)) {
  tmax[[i]] <- as.numeric(temp_recs[[i]]$data[,7])
}

tmin <- list()

for(i in 1: length(locations_NOW$loc)) {
  tmin[[i]] <- as.numeric(temp_recs[[i]]$data[,8])
}

# 4. Calculate cumulative degree days using max and min temps obtained by Vince

upT <- 12.8 + 19.8
baseT <- 12.8

DDs_NOW <- list()

for(i in 1: length(locations_NOW$loc)) {
  DDs_NOW[[i]] <- calc_dd_vec(tmax = tmax[[i]], tmin = tmin[[i]], 
                              lower_threshold = baseT, 
                              upper_threshold = upT, 
                              cutoff = "vertical")
  DDs_NOW[[i]] <- cumsum(DDs_NOW[[i]])
}

# Matching data collection days

data_clean$NOW_DD <- rep(NA, length(data_clean$loc))


for(i in 1: length(locations_NOW$loc)) {
  data_clean$NOW_DD[which(data_clean$latitude == locations_NOW$latitude[i] & 
                            data_clean$longitude == locations_NOW$longitude[i] &
                            data_clean$year == locations_NOW$year[i])] <-
    DDs_NOW[[i]][data_clean$julian[which(data_clean$latitude == 
                                           locations_NOW$latitude[i] &
                                           data_clean$longitude == 
                                           locations_NOW$longitude[i] &
                                           data_clean$year == 
                                           locations_NOW$year[i])]]
}

plot(data_clean$DD_vince, data_clean$NOW_DD)
abline(0, 1)


plot(data_clean$DD_vince, data_clean$moths)

plot(data_clean$NOW_DD, data_clean$moths)


