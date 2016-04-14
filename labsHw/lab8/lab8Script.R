rm(list = ls())
setwd("~/Dropbox/cornell/ORIE4740/labsHw/lab8/")
library(ggplot2)  
library(ISLR)
library(reshape2)

hospitals = read.csv("hospital.csv",header=T)

  orthoClust = subset.data.frame(hospitals, select = -c(ZIP,HID,CITY,STATE,TH,TRAUMA,REHAB,SALESY,SALES12))
orthoClust = log(orthoClust + 1)
orthoClust = scale(orthoClust)
orthoClust = as.data.frame(orthoClust)

minOrthoKMeans = c()
orthoKmeans = kmeans(orthoClust,3)
minCost = sum(orthoKmeans$withinss)
print(minCost)
for(i in 1:20)
{
  orthoKmeansTemp = kmeans(orthoClust,3)
  foo = sum(orthoKmeansTemp$withinss)
  if(foo<minCost)
  {
    minCost = foo
    orthoKmeans = orthoKmeansTemp
  }
}

print(sum(orthoKmeans$withinss))

print(orthoKmeans$centers)

pairs(orthoClust,col = orthoKmeans$cluster)
#print(orth)
#print(orth)
#pairs(orthoClust)

#ggplot(hospitals,aes(x = value))

# a <- ggplot(data = msleep,aes(x = bodywt,y = sleep_total))
# a <- a + geom_point()
# a <- a + xlab("Body Weight") + ylab("Total Hours Sleep") + ggtitle("Some Sleep Data")
# a
# 
# 
# density(hospitals)
# #h = ggplot(data = hospitals,aes(x = value))
#h = melt(orthoClust, id = 1)
#ggplot(h,aes(x=value,color = variable)) + facet_wrap(~variable,scales = "free_x") + geom_histogram()
# 
# melted = melt(orthoClust,id.vars='refseq')
# ggplot(melted, aes(value)) + geom_density(color = variable)

#REALLY IMPORTANT: http://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/