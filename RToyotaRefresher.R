setwd("~/Downloads/")

rm(list = ls())
corollas = read.csv("ToyotaCorolla.csv",header=T)
corollas2 = corollas[1:1436,]
corollas2$MetColor = as.factor(corollas2$MetColor)
corollas3 = corollas2[, c("Price","Age","KM","FuelType","HP","MetColor","Doors","Weight")]

nData = nrow( corollas3 )
nTrain = floor( .6*nData )
trainInd = sample( 1:nData, nTrain )
trainData = corollas3[trainInd,]
testData = corollas3[-trainInd,]


corollasLM = lm(formula = Price ~ ., data = trainData)
summary(corollasLM)

preds = predict.lm(object = corollasLM, newdata = testData)
hist(preds)
resids = testData$Price - preds
hist(resids)

rmse = sqrt(mean(resids^2))

print(rmse)

trainData2 = trainData
trainData2$Price = log( trainData$Price )
trainData2$KM = sqrt( trainData$KM )
trainData2$Weight = log( trainData$Weight - 950 )
testData2 = testData
testData2$KM = sqrt( testData$KM )
testData2$Weight = log( testData$Weight - 950 )

corollasLM = lm(formula = Price ~., data = trainData2)
preds = predict.lm( object = corollasLM,newdata = testData2)
resids = testData2$Price - exp(preds)
rmse = sqrt (mean (resids^2))
print(rmse)
