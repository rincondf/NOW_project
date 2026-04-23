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


