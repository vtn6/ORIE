setwd("~/Dropbox/cornell/ORIE4740/labsHw/lab2")
rm(list = ls())
accidentsData = read.table("accidentsData.csv",header = TRUE, sep = ",")
jointCounts = table(accidentsData$INJURY_CRASH, accidentsData$INT_HWY)
jointDist = t(t(jointCounts)/rowSums(jointCounts)) #transpose tha shit

print(jointDist)

accidentsData = accidentsData[,1:20]

trainData = accidentsData[-(1:1000),]


nb.train <- function(D = NULL){
  #======================================================================
  # Function: nb.train
  # from nbc-r, available on Google Code
  # GPL-2 license
  #                                              last updated:2005.11.10
  #                                                              by TSYo
  #----------------------------------------------------------------------
  # Description:
  #     training a naive Bayes model from a given data matrix.
  #----------------------------------------------------------------------
  # Input arguments:
  #     D
  #         A data matrix containing the training data.  Each row of D 
  #         contains one training example, with the class at the last 
  #         column.  
  #----------------------------------------------------------------------
  # Return objects:
  #     class.dist
  #         A vector contains the relative frequencies of the class labels.  
  #     attr.dist
  #         A list of matrices which contain the conditional distribution 
  #         of the corresponding attribute given the class label.
  #----------------------------------------------------------------------
  # Examples:
  #     nbWSBC <- nb.train(wsbc.dat)    # Train a naive Bayes classifier
  #     nbWSBC$class.dist               # Show the prob. of each class label
  #======================================================================
  # Checking arguments
  if(missing(D))
    stop("Please specify the matrix containing training data.")
  #    
  #----------------------------------------------------------------------
  # Probability calculation
  dimD = dim(D)
  nRow = dimD[1]                    # size of training sample
  nAttr = dimD[2] -1                # number of attributes
  cRow  = dimD[2]                   # row number of the class label
  adist = NULL
  
  # Calculate Pr(C)
  cdist = table(D[,cRow])/nRow
  
  # Calculate Pr(A_j|C)=Pr(A_j,C)/Pr(C)
  for (j in 1:nAttr){
    tmpAdist = table(D[,cRow],D[,j])/nRow      # Pr(A_i,C)
    for(i in 1:dim(cdist)){
      tmpAdist[i,] = tmpAdist[i,]/cdist[i]   # Pr(A_i,C)/Pr(C)
    }
    adist = c(adist,list(tmpAdist))
  }
  # End of probability calculation
  #----------------------------------------------------------------------
  # Return results
  return(list(class.dist=cdist,attr.dist=adist))
}
nb.predict <- function(D = NULL,nb = NULL){
  #======================================================================
  # Function: nb.predict
  # from nbc-r, available on Google Code
  #                                              last updated:2005.10.22
  #                                                              by TSYo
  #----------------------------------------------------------------------
  # Usage:
  #     nb.predict(D, nb)
  #----------------------------------------------------------------------
  # Description:
  #     predicting the given data based on the given naive Bayes model.
  #----------------------------------------------------------------------
  # Input arguments:
  #     D
  #         A data matrix containing the data to be predicted.  The last
  #         column contains the true class labels to be predicted.
  #     nb
  #         A naive Bayes model used to make predictions.  
  #----------------------------------------------------------------------
  # Return objects:
  #     class.pred
  #         A vector contains the predicted class labels.  
  #     conf.mat
  #         The confusion matrix, i.e. a cross-table of the true class 
  #         labels and the predicted class labels.
  #     error.rate
  #         An estimate of the error rate of nb based on the predictive
  #         accuracy of nb on D.
  #----------------------------------------------------------------------
  # Examples:
  #     
  #======================================================================
  # Checking arguments
  if(missing(D))
    stop("Please specify the matrix containing testing data.")
  if(missing(nb))
    stop("Please specify the naive Bayes model to be used for prediction.")
  #    
  #----------------------------------------------------------------------
  # Parameters
  #  From testing data
  dimD = dim(D)
  dataRow = dimD[1]                      # size of the testing data
  dataCol = dimD[2]                      # number of attributes + 1 (for class labels)
  #  From the naive Bayes model
  nbPrClass = nb$class.dist              # Prob. of each class label provided by the model
  nbPrAttr = nb$attr.dist                # Prob. of each attribute with its possible values
  nClass = dim(nbPrClass)                # Number of class labels
  nAttr  = length(nbPrAttr)              # Number of attributes
  #  For classification    
  # Matrix contains the prob. of each class for each row
  prClass = matrix(1, ncol = nClass, nrow = dataRow)
  #
  #----------------------------------------------------------------------
  # Probability calculation
  #   Pr(C=i|Aj) = Pr(C=i)*Prod(j){Pr(Aj|C=i)}/Sum(i){Pr(C=i)*prod{Pr(Aj|C=i)}}
  #     (1) Pr(C=i) can be looked up in nbPrClass[i]
  #     (2) Pr(Aj|C=i) can be looked up in nbPrAttr[[#Attr]][i,ValOfAttr]
  #     (3) The denominator is a constant for each class, can be ignored
  #
  for (i in 1:nClass){                   # Value of class
    for (j in 1:dataRow){              # For each records
      for (k in 1:nAttr){            # For each attribute
        # the following has been altered by Dawn Woodard on 09/07/08
        l = as.character( D[j,k] ) # Value of the attribute
        thisAttrClasses = colnames(nbPrAttr[[k]])
        # Prod(j){Pr(Aj|C=i)}
        if( sum( l == thisAttrClasses ) == 0 ){
          # predictor value not in training set
          prClass[j,i] = 0
        } else {
          prClass[j,i] = prClass[j,i]*nbPrAttr[[k]][i, which(l==thisAttrClasses) ]
        }
      }
      # Pr(C=i)*Prod(j){Pr(Aj|C=i)}
      prClass[j,i] = nbPrClass[i] * prClass[j,i]
    }
  }
  # End of probability calculation
  #----------------------------------------------------------------------
  # Predicting the class label:
  #   Select the class label with maximum probability.  If more than one
  #   class have max prob, select the first one.
  predClass = rep(0,dataRow)
  # the following has been altered by Dawn Woodard on 09/07/08
  classNames = names( nbPrClass )
  for (i in 1:dataRow){
    classInd = which(prClass[i,] == max(prClass[i,]))[1]
    predClass[i] = classNames[ classInd ]
  }
  #----------------------------------------------------------------------
  # Creating confusion martix:
  #   
  trueClass = as.character(D[,dataCol])
  confuMatrix = table(trueClass,predClass)
  #----------------------------------------------------------------------
  # Calculating error rate:
  #   
  err = sum(abs(predClass != trueClass),na.rm=T)/dataRow
  #----------------------------------------------------------------------
  # Return results
  return(list(class.pred=predClass ,conf.mat=confuMatrix ,error.rate=err ))
}

fooModel = nb.train(trainData)
accidentPrediction = nb.predict(D = accidentsData[1:1000,] , nb = fooModel)
