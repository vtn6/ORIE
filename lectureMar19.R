rm(list = ls())
library(MASS)
setwd("~/Downloads/")
credit = read.table("crx.data", sep=",",na.strings="?")
hist(credit$V15)
credit$logV15 = log(credit$V15 + 1)
dens = density(x = credit$logV15,na.rm=T,bw = .3)
dens2 = density(x = credit$logV15,na.rm=T)
plot(dens)
plot(dens2)
plot(credit$V11, credit$logV15)
miss = is.na(credit$V11) | is.na(credit$logV15)
dens3 = kde2d(x = credit$V11[!miss], y = credit$logV15[!miss],n = 200, h = c(10,1.4))
image(x = dens3$x,
      y = dens3$y,
      z = dens3$z,
      zlim = c(0,0.007))

points(x = credit$V11[!miss] , y = credit$logV15[!miss])

densPlusV15 = density(credit$logV15[credit$V16=="+"], na.rm =T,
                      from = min(credit$logV15), to=max(credit$logV15),
                      n = 512)
densMinusV15 = density(credit$logV15[credit$V16=="-"],na.rm = T,
                       from = min(credit$logV15), to = max(credit$logV15),
                       n = 512)

par(mfrow = c(3,1))
plot(densPlusV15,main = "")
plot(densMinusV15,main = "")
