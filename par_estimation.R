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



# Almonds

AlmondsDDs <- rep(NOW_almond$NOW_DD, round(NOW_almond$pmoths*1000))

hist(AlmondsDDs, breaks = seq(0, 2100, 15))


library(mixtools)

Alm_mix <- weibullRMM_SEM(AlmondsDDs, maxit = 500, k = 4)


Alm_mix1 <- gammamixEM(AlmondsDDs, maxit = 500, k = 4)

Alm_mix2 <- normalmixEM(AlmondsDDs, maxit = 500, k = 4)




hist(AlmondsDDs, breaks = seq(0, 2100, 15), freq = FALSE)
lines(seq(0, 2100, 0.01), dweibull(seq(0, 2100, 0.01), shape = Alm_mix$shape[1],
                                 scale = Alm_mix$scale[1]), lwd = 2)

lines(seq(0, 2100, 0.01), dweibull(seq(0, 2100, 0.01), shape = Alm_mix$shape[2],
                                    scale = Alm_mix$scale[2]), lwd = 2)

lines(seq(0, 2100, 0.01), dweibull(seq(0, 2100, 0.01), shape = Alm_mix$shape[3],
                                   scale = Alm_mix$scale[3]), lwd = 2)

lines(seq(0, 2100, 0.01), dweibull(seq(0, 2100, 0.01), shape = Alm_mix$shape[4],
                                   scale = Alm_mix$scale[4]), lwd = 2)

hist(AlmondsDDs, breaks = seq(0, 2100, 15), freq = FALSE)
lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Alm_mix1$gamma.pars[1, 1],
                                 scale = Alm_mix1$gamma.pars[2, 1])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Alm_mix1$gamma.pars[1, 2],
                                 scale = Alm_mix1$gamma.pars[2, 2])/2)

lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Alm_mix1$gamma.pars[1, 3],
                                 scale = Alm_mix1$gamma.pars[2, 3])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Alm_mix1$gamma.pars[1, 4],
                                 scale = Alm_mix1$gamma.pars[2, 4])/2)




hist(AlmondsDDs, breaks = seq(0, 2100, 15), freq = FALSE)
lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Alm_mix2$mu[1],
                                 sd = Alm_mix2$sigma[1])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Alm_mix2$mu[2],
                                sd = Alm_mix2$sigma[2])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Alm_mix2$mu[3],
                                sd = Alm_mix2$sigma[3])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Alm_mix2$mu[4],
                                sd = Alm_mix2$sigma[4])/2, lwd = 2)

