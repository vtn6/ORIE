library(arules)

#the average probability of downloading a particular document in a particular session is 0.0017
#the most frequently downloaded doc is 11d (356 times)
#In 11,615 sessions only 1 document was downloaded. In 2189 sessions exactly 2 docs were downloaded
#the average number of downlaods per sesision is 1.646

rules = apriori( data = Epub
                 )

#default support cutoff is 0.1, the default cutoff for confidence
#each document has a very low probability of being downloaded. Even the most common document has a download frequency of only 
#356/15729
rules = apriori(data = Epub, parameter = list(support = .0006, confidence = 0.8))
inspect(rules)

#documents 6e7,6e8,6e9 are frequently downloaded together and are probably closely related docs by 
#same authors, similarily for 574 764 3c4

#the lift values are very high, this means that there are ery strong 
#dependencies between the downloading of some documents