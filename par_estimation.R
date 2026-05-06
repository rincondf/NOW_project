load("males2013.RData")
load("males2014.RData")

tapply(males_2013$pmoths, males_2013$loc, sum)

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
rownames(NOW_almond) <- seq(1, length(NOW_almond$Date))

NOW_pistachio <- rbind(males_2013[which(males_2013$Crop == "Pistachio"), ], 
                    males_2014[which(males_2014$Crop == "Pistachio"), ])

rownames(NOW_pistachio) <- seq(1, length(NOW_pistachio$Date))

plot(NOW_almond$NOW_DD, NOW_almond$moths)
points(NOW_pistachio$NOW_DD, NOW_pistachio$moths, col = "red")


plot(NOW_almond$NOW_DD, NOW_almond$pmoths)
points(NOW_pistachio$NOW_DD, NOW_pistachio$pmoths, col = "red")


plot(NOW_almond$NOW_DD, NOW_almond$cpmoths)
points(NOW_pistachio$NOW_DD, NOW_pistachio$cpmoths, col = "red")



# Almonds

bad_almonds <- unique(NOW_almond$loc[which(NOW_almond$cpmoths > 0.70 & NOW_almond$NOW_DD < 540)])
NOW_almond <- NOW_almond[-which(NOW_almond$loc %in% bad_almonds), ]


AlmondsDDs <- rep(NOW_almond$NOW_DD, round(NOW_almond$pmoths*1000))

hist(AlmondsDDs, breaks = seq(0, 2100, 15))

plot(NOW_almond$NOW_DD, NOW_almond$pmoths)
plot(NOW_almond$NOW_DD, NOW_almond$cpmoths)

library(mixtools)

Alm_mix1 <- gammamixEM(AlmondsDDs, maxit = 2000, k = 4)

Alm_mix2 <- normalmixEM(AlmondsDDs, maxit = 2000, k = 4)




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
                                sd = Alm_mix2$sigma[2])/4, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Alm_mix2$mu[3],
                                sd = Alm_mix2$sigma[3])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Alm_mix2$mu[4],
                                sd = Alm_mix2$sigma[4])/2, lwd = 2)

hist(AlmondsDDs, breaks = seq(0, 2100, 15), freq = FALSE, ylim = c(0, 0.004))

AlmondsDDs1 <- AlmondsDDs[order(AlmondsDDs)]
hist(AlmondsDDs1[1: round(length(AlmondsDDs1) * Alm_mix2$lambda[1])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col = "red", add = T)

AlmondsDDs1A <- AlmondsDDs1[1: round(length(AlmondsDDs1) * Alm_mix2$lambda[1])]

hist(AlmondsDDs1[round(length(AlmondsDDs1) * Alm_mix2$lambda[1]) + 1: 
                   round(length(AlmondsDDs1) * Alm_mix2$lambda[2])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col  = "blue", add = T)

AlmondsDDs1B <- AlmondsDDs1[round(length(AlmondsDDs1) * Alm_mix2$lambda[1]) + 1: 
                              round(length(AlmondsDDs1) * Alm_mix2$lambda[2])]

hist(AlmondsDDs1[round(length(AlmondsDDs1) * Alm_mix2$lambda[1]) + 
                   round(length(AlmondsDDs1) * Alm_mix2$lambda[2]) + 1: 
                   round(length(AlmondsDDs1) * Alm_mix2$lambda[3])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col  = "brown", add = T)

AlmondsDDs1C <- AlmondsDDs1[round(length(AlmondsDDs1) * Alm_mix2$lambda[1]) + 
                              round(length(AlmondsDDs1) * Alm_mix2$lambda[2]) + 1: 
                              round(length(AlmondsDDs1) * Alm_mix2$lambda[3])]


hist(AlmondsDDs1[round(length(AlmondsDDs1) * Alm_mix2$lambda[1]) + 
                   round(length(AlmondsDDs1) * Alm_mix2$lambda[2]) + 
                   round(length(AlmondsDDs1) * Alm_mix2$lambda[3]) + 1: 
                   round(length(AlmondsDDs1) * Alm_mix2$lambda[4])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col  = "gold", add = T)


AlmondsDDs1D <- AlmondsDDs1[round(length(AlmondsDDs1) * Alm_mix2$lambda[1]) + 
                              round(length(AlmondsDDs1) * Alm_mix2$lambda[2]) + 
                              round(length(AlmondsDDs1) * Alm_mix2$lambda[3]) + 1: 
                              round(length(AlmondsDDs1) * Alm_mix2$lambda[4]) - 1]




library(bbmle)

MLL_Alm1 <- function(shape, scale) {
  -sum(dweibull(x, shape = shape, scale = scale, log = TRUE))
}

mod_Alm1 <- mle2(MLL_Alm1, start = list(shape = 3, scale  = 328), 
                data = list(x = AlmondsDDs1A))
summary(mod_Alm1)

mod_Alm2 <- mle2(MLL_Alm1, start = list(shape = 6, scale  = 800), 
                 data = list(x = AlmondsDDs1B))
summary(mod_Alm2)

mod_Alm3 <- mle2(MLL_Alm1, start = list(shape = 13, scale  = 1300), 
                 data = list(x = AlmondsDDs1C))
summary(mod_Alm3)

mod_Alm4 <- mle2(MLL_Alm1, start = list(shape = 17, scale  = 1700), 
                 data = list(x = AlmondsDDs1D))
summary(mod_Alm4)


hist(AlmondsDDs1A, freq = FALSE, xlim = c(0, 2100))
lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm1)[1],
                                   scale = coef(mod_Alm1)[2]), lwd = 2)


hist(AlmondsDDs1B, freq = FALSE, add = TRUE)
lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm2)[1],
                                   scale = coef(mod_Alm2)[2]), lwd = 2)







plot(NOW_almond$NOW_DD, NOW_almond$pmoths)
lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm1)[1],
                                 scale = coef(mod_Alm1)[2])*20, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm2)[1],
                                   scale = coef(mod_Alm2)[2])*10, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm3)[1],
                                   scale = coef(mod_Alm3)[2])*40, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm4)[1],
                                   scale = coef(mod_Alm4)[2])*40, lwd = 2)




# Pistachios

bad_pistachios <- unique(NOW_pistachio$loc[which(NOW_pistachio$cpmoths > 0.70 & NOW_pistachio$NOW_DD < 540)])
NOW_pistachio <- NOW_pistachio[-which(NOW_pistachio$loc %in% bad_pistachios), ]




PistachiosDDs <- rep(NOW_pistachio$NOW_DD, round(NOW_pistachio$pmoths*10000))

hist(PistachiosDDs, breaks = seq(0, 2100, 20))

plot(NOW_pistachio$NOW_DD, NOW_pistachio$pmoths, ylim = c(0, 0.25))

plot(NOW_pistachio$NOW_DD, NOW_pistachio$cpmoths)






library(mixtools)

Pis_mix <- weibullRMM_SEM(PistachiosDDs, maxit = 500, k = 4)


Pis_mix1 <- gammamixEM(PistachiosDDs, lambda = Alm_mix2$lambda, k = 4, maxit = 2000)

Pis_mix2 <- normalmixEM(PistachiosDDs, maxit = 2500, mu = Alm_mix2$mu, sigma = Alm_mix2$sigma, sd.constr = rep(mean(Alm_mix2$sigma), 4))




hist(PistachiosDDs, breaks = seq(0, 2100, 15), freq = FALSE)
lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Pis_mix1$gamma.pars[1, 1],
                                 scale = Pis_mix1$gamma.pars[2, 1])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Pis_mix1$gamma.pars[1, 2],
                                 scale = Pis_mix1$gamma.pars[2, 2])/2)

lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Pis_mix1$gamma.pars[1, 3],
                                 scale = Pis_mix1$gamma.pars[2, 3])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dgamma(seq(0, 2000, 0.01), shape = Pis_mix1$gamma.pars[1, 4],
                                 scale = Pis_mix1$gamma.pars[2, 4])/2)





hist(PistachiosDDs, breaks = seq(0, 2100, 15), freq = FALSE)
lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Pis_mix2$mu[1],
                                sd = Pis_mix2$sigma[1])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Pis_mix2$mu[2],
                                sd = Pis_mix2$sigma[2])/4, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Pis_mix2$mu[3],
                                sd = Pis_mix2$sigma[3])/2, lwd = 2)

lines(seq(0, 2000, 0.01), dnorm(seq(0, 2000, 0.01), mean = Pis_mix2$mu[4],
                                sd = Pis_mix2$sigma[4])/2, lwd = 2)



hist(PistachiosDDs, breaks = seq(0, 2100, 15), freq = FALSE, ylim = c(0, 0.005))

PistachiosDDs1 <- PistachiosDDs[order(PistachiosDDs)]
hist(PistachiosDDs1[1: round(length(PistachiosDDs1) * Pis_mix2$lambda[1])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col = "red", add = T)

PistachiosDDs1A <- PistachiosDDs1[1: round(length(PistachiosDDs1) * Pis_mix2$lambda[1])]

hist(PistachiosDDs1[round(length(PistachiosDDs1) * Pis_mix2$lambda[1]) + 1: 
                   round(length(PistachiosDDs1) * Pis_mix2$lambda[2])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col  = "blue", add = T)

PistachiosDDs1B <- PistachiosDDs1[round(length(PistachiosDDs1) * Pis_mix2$lambda[1]) + 1: 
                              round(length(PistachiosDDs1) * Pis_mix2$lambda[2])]

hist(PistachiosDDs1[round(length(PistachiosDDs1) * Pis_mix2$lambda[1]) + 
                   round(length(PistachiosDDs1) * Pis_mix2$lambda[2]) + 1: 
                   round(length(PistachiosDDs1) * Pis_mix2$lambda[3])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col  = "brown", add = T)

PistachiosDDs1C <- PistachiosDDs1[round(length(PistachiosDDs1) * Pis_mix2$lambda[1]) + 
                              round(length(PistachiosDDs1) * Pis_mix2$lambda[2]) + 1: 
                              round(length(PistachiosDDs1) * Pis_mix2$lambda[3])]


hist(PistachiosDDs1[round(length(PistachiosDDs1) * Pis_mix2$lambda[1]) + 
                   round(length(PistachiosDDs1) * Pis_mix2$lambda[2]) + 
                   round(length(PistachiosDDs1) * Pis_mix2$lambda[3]) + 1: 
                   round(length(PistachiosDDs1) * Pis_mix2$lambda[4])], 
     breaks = seq(0, 2100, 15), freq = FALSE, col  = "gold", add = T)


PistachiosDDs1D <- PistachiosDDs1[round(length(PistachiosDDs1) * Pis_mix2$lambda[1]) + 
                              round(length(PistachiosDDs1) * Pis_mix2$lambda[2]) + 
                              round(length(PistachiosDDs1) * Pis_mix2$lambda[3]) + 1: 
                              round(length(PistachiosDDs1) * Pis_mix2$lambda[4])]




library(bbmle)

MLL_Pis1 <- function(shape, scale) {
  -sum(dweibull(x, shape = shape, scale = scale, log = TRUE))
}

mod_Pis1 <- mle2(MLL_Pis1, start = list(shape = 2, scale  = 260), 
                 data = list(x = PistachiosDDs1A))
summary(mod_Pis1)

mod_Pis2 <- mle2(MLL_Pis1, start = list(shape = 6, scale  = 800), 
                 data = list(x = PistachiosDDs1B))
summary(mod_Pis2)

mod_Pis3 <- mle2(MLL_Pis1, start = list(shape = 13, scale  = 1300), 
                 data = list(x = PistachiosDDs1C))
summary(mod_Pis3)

mod_Pis4 <- mle2(MLL_Pis1, start = list(shape = 17, scale  = 1700), 
                 data = list(x = PistachiosDDs1D))
summary(mod_Pis4)


plot(NOW_pistachio$NOW_DD, NOW_pistachio$pmoths)
lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis1)[1],
                                   scale = coef(mod_Pis1)[2])*20, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis2)[1],
                                   scale = coef(mod_Pis2)[2])*20, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis3)[1],
                                   scale = coef(mod_Pis3)[2])*40, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis4)[1],
                                   scale = coef(mod_Pis4)[2])*40, lwd = 2)




#####################

almonds_mod <- function(x) {
  
  (1/4) * (pweibull(x, shape = coef(mod_Alm1)[1],
                    scale = coef(mod_Alm1)[2]) + 
             pweibull(x, shape = coef(mod_Alm2)[1],
                      scale = coef(mod_Alm2)[2]) +
             pweibull(x, shape = coef(mod_Alm3)[1],
                      scale = coef(mod_Alm3)[2]) +
             pweibull(x, shape = coef(mod_Alm4)[1],
                      scale = coef(mod_Alm4)[2]))
}


plot(NOW_almond$NOW_DD, NOW_almond$cpmoths)

lines(seq(0, 2100), (almonds_mod(seq(0, 2100))))


pistachios_mod <- function(x) {
  
  (1/4) * (pweibull(x, shape = coef(mod_Pis1)[1],
                    scale = coef(mod_Pis1)[2]) + 
             pweibull(x, shape = coef(mod_Pis2)[1],
                      scale = coef(mod_Pis2)[2]) +
             pweibull(x, shape = coef(mod_Pis3)[1],
                      scale = coef(mod_Pis3)[2]) +
             pweibull(x, shape = coef(mod_Pis4)[1],
                      scale = coef(mod_Pis4)[2]))
}


plot(NOW_pistachio$NOW_DD, NOW_pistachio$cpmoths)

lines(seq(0, 2100), (pistachios_mod(seq(0, 2100))))















plot(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis1)[1],
                                   scale = coef(mod_Pis1)[2])*25, lwd = 2, type = "l", lty = 2, ylim = c(0, 0.15))


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis2)[1],
                                   scale = coef(mod_Pis2)[2])*20, lwd = 2, lty = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis3)[1],
                                   scale = coef(mod_Pis3)[2])*40, lwd = 2, lty = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Pis4)[1],
                                   scale = coef(mod_Pis4)[2])*40, lwd = 2, lty = 2)



lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm1)[1],
                                   scale = coef(mod_Alm1)[2])*20, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm2)[1],
                                   scale = coef(mod_Alm2)[2])*18, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm3)[1],
                                   scale = coef(mod_Alm3)[2])*36, lwd = 2)


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = coef(mod_Alm4)[1],
                                   scale = coef(mod_Alm4)[2])*40, lwd = 2)





lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = 3.122675,
                                   scale = 328.0322) * 19.5, lwd = 2, col = "blue")

lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = 2.318188,
                                   scale = 260.0913) * 19.5, lwd = 2, col = "blue", lty = 2)

lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = 6.676009,
                                   scale = 858.6814) * 23, lwd = 2, col = "blue")


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = 13.22359,
                                   scale = 1362.8031) * 40, lwd = 2, col = "blue")


lines(seq(0, 2000, 0.01), dweibull(seq(0, 2000, 0.01), shape = 17.11678,
                                   scale = 1766.1618) * 29, lwd = 2, col = "blue")



