rm(list = ls())
setwd("~/Downloads/")
votes = dget(file="votes.repub")

votes = as.data.frame(votes)

dropList = c("Alaska","Hawaii")

votes = votes[-c(2,11),]
votes = votes[,-c(1:15)]

votesClust = kmeans(votes,3,nstart=200)

print(votesClust$cluster)

# Indicies stuff ----------------------------------------------------------


# 
# 
ind1 = 0
ind2 = 0
for(i in 1: nrow(votes))
{
  if(row.names(votes)[i] == "Alaska" )
  {
    ind1 = i
  }
  
  if(row.names(votes)[i] == "Hawaii" )
  {
    ind2 = i
  }
  
  
  
}

# stuff -------------------------------------------------------------------

