rm(list = ls())
setwd("~/Dropbox/cornell/ORIE4740/labsHw/lab7/")
library(MASS)
ambulances = read.table("ambulances.txt")  
plot(ambulances$V1,ambulances$V2,pch=".")

sampleInd = sample(1:nrow(ambulances),1000) #incices for the test set
testAmbulances = ambulances[sampleInd,]
trainAmbulances = ambulances[-sampleInd,]


# make a heat map ---------------------------------------------------------


dens = kde2d( x = trainAmbulances$V1,
              y = trainAmbulances$V2,
              n = 200 )
image(x = dens$x,
      y = dens$y,
      z = dens$z)

points( x = trainAmbulances[,1],
        y = trainAmbulances[,2],
        pch = "."
)

# Make an adjusted kernel heatmap -----------------------------------------
dens = kde2d( x = trainAmbulances$V1,
              y = trainAmbulances$V2,
              n = 200,
              h =,c(0.4,0.4))
image(x = dens$x,
      y = dens$y,
      z = dens$z)

points( x = trainAmbulances[,1],
        y = trainAmbulances[,2],
        pch = "."
)

dens = kde2d( x = trainAmbulances$V1,
              y = trainAmbulances$V2,
              n = 200,
              h =,c(14,14))
image(x = dens$x,
      y = dens$y,
      z = dens$z)

points( x = trainAmbulances[,1],
        y = trainAmbulances[,2],
        pch = "."
)


# logScoring --------------------------------------------------------------



bw = exp((-4:4)/2)
logScore = rep(0,length(bw))
nTest = nrow(testAmbulances)
for(k in 1:length(bw))
{
  for(i in 1:nTest )
  {
    densTerm = dnorm( x = testAmbulances[i,1], mean = trainAmbulances[,1], sd = bw[k]) *
      dnorm(x = testAmbulances[i,2], mean = trainAmbulances[,2], sd = bw[k])
    densThisObs = mean (densTerm)
    logScore[k] = logScore[k] + log(densThisObs)
  }
}
logScore = logScore/nTest

plot(bw,logScore)