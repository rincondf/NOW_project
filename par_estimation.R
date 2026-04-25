load("males2013.RData")
load("males2014.RData")

plot(males_2013$NOW_DD[which(males_2013$Crop == "Almond")], 
     males_2013$moths[which(males_2013$Crop == "Almond")])

points(males_2013$NOW_DD[which(males_2013$Crop == "Pistachio")], 
       males_2013$moths[which(males_2013$Crop == "Pistachio")], col = "red")


plot(males_2014$NOW_DD[which(males_2014$Crop == "Almond")], 
     males_2014$moths[which(males_2014$Crop == "Almond")])

points(males_2014$NOW_DD[which(males_2014$Crop == "Pistachio")], 
       males_2014$moths[which(males_2014$Crop == "Pistachio")], col = "red")


NOW_almond <- rbind(males_2013[which(males_2013$Crop == "Almond"), ], 
                    males_2014[which(males_2014$Crop == "Almond"), ])


NOW_pistachio <- rbind(males_2013[which(males_2013$Crop == "Pistachio"), ], 
                    males_2014[which(males_2014$Crop == "Pistachio"), ])



plot(NOW_almond$NOW_DD, NOW_almond$moths)
points(NOW_pistachio$NOW_DD, NOW_pistachio$moths, col = "red")


plot(NOW_almond$NOW_DD, NOW_almond$pmoths)
points(NOW_pistachio$NOW_DD, NOW_pistachio$pmoths, col = "red")
