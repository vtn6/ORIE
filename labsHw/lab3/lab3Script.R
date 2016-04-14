rm(list = ls())
setwd("~/Dropbox/cornell/ORIE4740/labsHw/lab3")
source("naiveBayesThreshold.R")


# reading Data ------------------------------------------------------------
incomeTrain = read.table("adult.data.preproc", sep = "," , na.strings=" ?")
incomeTest = read.table("adult.test.preproc", sep = "," , na.strings=" ?")
names(incomeTrain) = c("age","workclass","education","marital","race","sex","inc")
names(incomeTest) = c("age","workclass","education","marital","race","sex","inc")

# 
# incomeBayesModel = nb.train(incomeTrain)
# incomePrediction = nb.predict(D=incomeTest,nb=incomeBayesModel,threshold=0.5)

# reformatting incomeTrain ------------------------------------------------
ageMarital = 
  1*((incomeTrain$age == TRUE) & (incomeTrain$marital == " Divorced")) + 
  2*((incomeTrain$age == TRUE) & (incomeTrain$marital == " Married/Widowed")) +
  3*((incomeTrain$age == TRUE) & (incomeTrain$marital == " Never-married")) +
  4*((incomeTrain$age == FALSE) & (incomeTrain$marital == " Divorced")) +
  5*((incomeTrain$age == FALSE) & (incomeTrain$marital == " Married/Widowed")) +
  6*((incomeTrain$age == FALSE) & (incomeTrain$marital == " Never-married")) 

incomeFoo = incomeTrain$inc
incomeTrain = subset(incomeTrain, select = -c(1,4,7))
incomeTrain$ageMarital = as.factor(ageMarital)
incomeTrain$inc = incomeFoo

# reformatting incomeTest -------------------------------------------------
ageMarital = 
  1*((incomeTest$age == TRUE) & (incomeTest$marital == " Divorced")) + 
  2*((incomeTest$age == TRUE) & (incomeTest$marital == " Married/Widowed")) +
  3*((incomeTest$age == TRUE) & (incomeTest$marital == " Never-married")) +
  4*((incomeTest$age == FALSE) & (incomeTest$marital == " Divorced")) +
  5*((incomeTest$age == FALSE) & (incomeTest$marital == " Married/Widowed")) +
  6*((incomeTest$age == FALSE) & (incomeTest$marital == " Never-married")) 
incomeFoo = incomeTest$inc
incomeTest = subset(incomeTest, select = -c(1,4,7))
incomeTest$ageMarital = as.factor(ageMarital)
incomeTest$inc = incomeFoo

# performing naive Bayes --------------------------------------------------
incomeBayesModel = nb.train(incomeTrain)
incomePrediction = nb.predict(D=incomeTest,nb=incomeBayesModel)
print(incomePrediction)

# incomeTrain = subset(incomeTrain ,select = -c(" marital"," age"))