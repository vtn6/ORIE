
# Data Processing ---------------------------------------------------------
rm(list = ls())
setwd("~/Dropbox/cornell/ORIE4740/labsHw/lab6/")
universities = read.csv("Universities.csv",header=T)
universities$State = NULL
universities$Public.1.or.Private.2 = NULL
universities$num.applications.accepted = universities$num.applications.accepted/universities$num.applications.received

# Remove variables (columns) with a high percentage of missing data -----------------
foo = c()
for (i in 1:21)
{
  #print(sum(is.na(universities[[i]])))
  if (sum(is.na(universities[[i]])) > 1302*0.2  ) 
  {
    #get all of the indeces and then remove AFTER the for loop
    foo = c(foo,i)
  }
}
universities = universities[-foo] #drop the columns that have too many NAs


# Remove observations (rows) with a missing data --------------------------
foo = c()
for(i in 1:dim(universities)[1])
{
    if(sum(is.na(universities[i,])  ) >=1    )
  {
    foo = c(foo,i)
  }
}
universities = universities[-foo,]

foo = universities[,-1]
#foo = scale(foo)


# scaling -----------------------------------------------------------------
for(i in 1: ncol(foo))
{
  tempCol = foo[,i]
  foo[,i] = scale(tempCol)
}
universitiesScaled = foo


# perform PCA -------------------------------------------------------------

collegePCA = prcomp(universitiesScaled)


# plotting ----------------------------------------------------------------

plot(collegePCA$x[,1],collegePCA$x[,2],
     xlim = c(-10,10),
     ylim = c(-10,10),
     main = "PC1 vs PC2",
     xlab = "PC 1",
     ylab = "PC 2",
     )

identify(x = collegePCA$x[,1],y = collegePCA$x[,2],labels = universities[,1])

v1 = as.numeric(collegePCA$rotation[,1])
v2 = as.numeric(collegePCA$rotation[,2])
x = as.numeric(universitiesScaled[1,])