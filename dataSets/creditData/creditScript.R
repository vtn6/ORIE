rm(list = ls())
setwd("~/Dropbox/cornell/ORIE4740/creditData/")
source("naiveBayesMissing.R")
# creditNames = read.table("crx.names")
creditData = read.table("crx.data",sep=",",na.strings="?")
nData = dim(creditData)[1]

# creating sampling indicies -----------------------------------------------
trainInd = sample(1:nData,460)
trainData = creditData[trainInd,]
testData = creditData[-trainInd,]

#use this data for unmodified model
trainData2 = trainData
testData2 = testData




# reformatting the data ---------------------------------------------------

medianV2 = median(trainData$V2,na.rm=T)
trainData$V2 = as.factor(trainData$V2 > medianV2)
medianV3 <- median(trainData$V3, na.rm = T)
trainData$V3 <- as.factor( trainData$V3 > medianV3 ) 
medianV8 <- median(trainData$V8, na.rm = T)
trainData$V8 <- as.factor( trainData$V8 > medianV8 ) 
medianV11 <- median(trainData$V11, na.rm = T)
trainData$V11 <- as.factor( trainData$V11 > medianV11 ) 
medianV14 <- median(trainData$V14, na.rm = T)
trainData$V14 <- as.factor( trainData$V14 > medianV14 ) 
medianV15 <- median(trainData$V15, na.rm = T)
trainData$V15 <- as.factor( trainData$V15 > medianV15 ) 

testData$V2 <- as.factor( testData$V2 > medianV2 ) 
testData$V3 <- as.factor( testData$V3 > medianV3 )
testData$V8 <- as.factor( testData$V8 > medianV8 ) 
testData$V11 <- as.factor( testData$V11 > medianV11 )
testData$V14 <- as.factor( testData$V14 > medianV14 )
testData$V15 <- as.factor( testData$V15 > medianV15 )

# training and producing predictions --------------------------------------

creditTrained = nb.train(D = trainData)
errorVector = c()
truePositiveVector = c(1)
falsePositiveVector = c(1)

creditTrained2 = nb.train(D = trainData2)
errorVector2 = c()
truePositiveVector2 = c(1)
falsePositiveVector2 = c(1)



# Prediction --------------------------------------------------------------
for(i in seq(0.001,0.009,0.001)){
  print(i)
  fooThreshold = i/10
  fooPrediction = nb.predict(D=testData,nb=creditTrained,threshold = fooThreshold )
  fooTruePositiveRate = fooPrediction$conf.mat[2,2]/( fooPrediction$conf.mat[2,2] + fooPrediction$conf.mat[2,1]  )
  fooFalsePositiveRate = fooPrediction$conf.mat[1,2]/( fooPrediction$conf.mat[1,1] + fooPrediction$conf.mat[1,2]  )
  
  truePositiveVector = c(truePositiveVector,fooTruePositiveRate)
  falsePositiveVector = c(falsePositiveVector,fooFalsePositiveRate)
  errorVector = c(errorVector,fooPrediction$error.rate)
  print(fooPrediction$error.rate)
  
  fooPrediction2 = nb.predict(D=testData2,nb=creditTrained2,threshold = fooThreshold )
  fooTruePositiveRate2 = fooPrediction2$conf.mat[2,2]/( fooPrediction2$conf.mat[2,2] + fooPrediction2$conf.mat[2,1]  )
  fooFalsePositiveRate2 = fooPrediction2$conf.mat[1,2]/( fooPrediction2$conf.mat[1,1] + fooPrediction2$conf.mat[1,2]  )
  
  truePositiveVector2 = c(truePositiveVector2,fooTruePositiveRate2)
  falsePositiveVector2 = c(falsePositiveVector,fooFalsePositiveRate2)
  errorVector2 = c(errorVector2,fooPrediction2$error.rate)
  
}

for(i in seq(0.01,1,0.1)){
  print(i)
  fooThreshold = i/10
  fooPrediction = nb.predict(D=testData,nb=creditTrained,threshold = fooThreshold )
  fooTruePositiveRate = fooPrediction$conf.mat[2,2]/( fooPrediction$conf.mat[2,2] + fooPrediction$conf.mat[2,1]  )
  fooFalsePositiveRate = fooPrediction$conf.mat[1,2]/( fooPrediction$conf.mat[1,1] + fooPrediction$conf.mat[1,2]  )
  
  truePositiveVector = c(truePositiveVector,fooTruePositiveRate)
  falsePositiveVector = c(falsePositiveVector,fooFalsePositiveRate)
  errorVector = c(errorVector,fooPrediction$error.rate)
  print(fooPrediction$error.rate)
  
  fooPrediction2 = nb.predict(D=testData2,nb=creditTrained2,threshold = fooThreshold )
  fooTruePositiveRate2 = fooPrediction2$conf.mat[2,2]/( fooPrediction2$conf.mat[2,2] + fooPrediction2$conf.mat[2,1]  )
  fooFalsePositiveRate2 = fooPrediction2$conf.mat[1,2]/( fooPrediction2$conf.mat[1,1] + fooPrediction2$conf.mat[1,2]  )
  
  truePositiveVector2 = c(truePositiveVector2,fooTruePositiveRate2)
  falsePositiveVector2 = c(falsePositiveVector,fooFalsePositiveRate2)
  errorVector2 = c(errorVector2,fooPrediction2$error.rate)
  
}

for(i in seq(1,7,0.5)){
  print(i)
  fooThreshold = i/10
  fooPrediction = nb.predict(D=testData,nb=creditTrained,threshold = fooThreshold )
  fooTruePositiveRate = fooPrediction$conf.mat[2,2]/( fooPrediction$conf.mat[2,2] + fooPrediction$conf.mat[2,1]  )
  fooFalsePositiveRate = fooPrediction$conf.mat[1,2]/( fooPrediction$conf.mat[1,1] + fooPrediction$conf.mat[1,2]  )
  
  truePositiveVector = c(truePositiveVector,fooTruePositiveRate)
  falsePositiveVector = c(falsePositiveVector,fooFalsePositiveRate)
  errorVector = c(errorVector,fooPrediction$error.rate)
  print(fooPrediction$error.rate)
  
  fooPrediction2 = nb.predict(D=testData2,nb=creditTrained2,threshold = fooThreshold )
  fooTruePositiveRate2 = fooPrediction2$conf.mat[2,2]/( fooPrediction2$conf.mat[2,2] + fooPrediction2$conf.mat[2,1]  )
  fooFalsePositiveRate2 = fooPrediction2$conf.mat[1,2]/( fooPrediction2$conf.mat[1,1] + fooPrediction2$conf.mat[1,2]  )
  
  truePositiveVector2 = c(truePositiveVector2,fooTruePositiveRate2)
  falsePositiveVector2 = c(falsePositiveVector,fooFalsePositiveRate2)
  errorVector2 = c(errorVector2,fooPrediction2$error.rate)
  
}

for(i in seq(7,8,0.1)){
  print(i)
  fooThreshold = i/10
  fooPrediction = nb.predict(D=testData,nb=creditTrained,threshold = fooThreshold )
  fooTruePositiveRate = fooPrediction$conf.mat[2,2]/( fooPrediction$conf.mat[2,2] + fooPrediction$conf.mat[2,1]  )
  fooFalsePositiveRate = fooPrediction$conf.mat[1,2]/( fooPrediction$conf.mat[1,1] + fooPrediction$conf.mat[1,2]  )
  
  truePositiveVector = c(truePositiveVector,fooTruePositiveRate)
  falsePositiveVector = c(falsePositiveVector,fooFalsePositiveRate)
  errorVector = c(errorVector,fooPrediction$error.rate)
  print(fooPrediction$error.rate)
  
  fooPrediction2 = nb.predict(D=testData2,nb=creditTrained2,threshold = fooThreshold )
  fooTruePositiveRate2 = fooPrediction2$conf.mat[2,2]/( fooPrediction2$conf.mat[2,2] + fooPrediction2$conf.mat[2,1]  )
  fooFalsePositiveRate2 = fooPrediction2$conf.mat[1,2]/( fooPrediction2$conf.mat[1,1] + fooPrediction2$conf.mat[1,2]  )
  
  truePositiveVector2 = c(truePositiveVector2,fooTruePositiveRate2)
  falsePositiveVector2 = c(falsePositiveVector,fooFalsePositiveRate2)
  errorVector2 = c(errorVector2,fooPrediction2$error.rate)
  
}

for(i in seq(8,9.9,0.1)){
  print(i)
  fooThreshold = i/10
  fooPrediction = nb.predict(D=testData,nb=creditTrained,threshold = fooThreshold )
  fooTruePositiveRate = fooPrediction$conf.mat[2,2]/( fooPrediction$conf.mat[2,2] + fooPrediction$conf.mat[2,1]  )
  fooFalsePositiveRate = fooPrediction$conf.mat[1,2]/( fooPrediction$conf.mat[1,1] + fooPrediction$conf.mat[1,2]  )
  
  truePositiveVector = c(truePositiveVector,fooTruePositiveRate)
  falsePositiveVector = c(falsePositiveVector,fooFalsePositiveRate)
  errorVector = c(errorVector,fooPrediction$error.rate)
  print(fooPrediction$error.rate)
  
  fooPrediction2 = nb.predict(D=testData2,nb=creditTrained2,threshold = fooThreshold )
  fooTruePositiveRate2 = fooPrediction2$conf.mat[2,2]/( fooPrediction2$conf.mat[2,2] + fooPrediction2$conf.mat[2,1]  )
  fooFalsePositiveRate2 = fooPrediction2$conf.mat[1,2]/( fooPrediction2$conf.mat[1,1] + fooPrediction2$conf.mat[1,2]  )
  
  truePositiveVector2 = c(truePositiveVector2,fooTruePositiveRate2)
  falsePositiveVector2 = c(falsePositiveVector,fooFalsePositiveRate2)
  errorVector2 = c(errorVector2,fooPrediction2$error.rate)
}

for(i in seq(9.9,9.99,0.01)){
  print(i)
  fooThreshold = i/10
  fooPrediction = nb.predict(D=testData,nb=creditTrained,threshold = fooThreshold )
  fooTruePositiveRate = fooPrediction$conf.mat[2,2]/( fooPrediction$conf.mat[2,2] + fooPrediction$conf.mat[2,1]  )
  fooFalsePositiveRate = fooPrediction$conf.mat[1,2]/( fooPrediction$conf.mat[1,1] + fooPrediction$conf.mat[1,2]  )
  
  truePositiveVector = c(truePositiveVector,fooTruePositiveRate)
  falsePositiveVector = c(falsePositiveVector,fooFalsePositiveRate)
  errorVector = c(errorVector,fooPrediction$error.rate)
  print(fooPrediction$error.rate)
  
  fooPrediction2 = nb.predict(D=testData2,nb=creditTrained2,threshold = fooThreshold )
  fooTruePositiveRate2 = fooPrediction2$conf.mat[2,2]/( fooPrediction2$conf.mat[2,2] + fooPrediction2$conf.mat[2,1]  )
  fooFalsePositiveRate2 = fooPrediction2$conf.mat[1,2]/( fooPrediction2$conf.mat[1,1] + fooPrediction2$conf.mat[1,2]  )
  
  truePositiveVector2 = c(truePositiveVector2,fooTruePositiveRate2)
  falsePositiveVector2 = c(falsePositiveVector,fooFalsePositiveRate2)
  errorVector2 = c(errorVector2,fooPrediction2$error.rate)
}

truePositiveVector = c(truePositiveVector,0)
falsePositiveVector = c(falsePositiveVector,0)

truePositiveVector2 = c(truePositiveVector2,0)
falsePositiveVector2 = c(falsePositiveVector2,0)

plot(errorVector,xlab="threshold")
plot(errorVector2,xlab="threshold")

plot(falsePositiveVector,truePositiveVector,xlab="False Positive Rate",ylab="True Positive Rate",type="l")

plot(falsePositiveVector2,truePositiveVector2,xlab="False Positive Rate",ylab="True Positive Rate",type="l")

plot(falsePositiveVector2,truePositiveVector2,xlab="False Positive Rate",ylab="True Positive Rate",type="l",col="red")
lines(falsePositiveVector,truePositiveVector,col="green")

