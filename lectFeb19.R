library(nnet)
rm(list = ls())
data(iris)
samp = sample(1:150,75)
iris.net = nnet(Species ~., data= iris[samp,],size = 2,rang = 0.1,decay=5e-4,maxit =200)
# iris.net = nnet(Species~., data = iris[samp],size = 2,rang = 0.1,decay = 5e-4,maxit = 200)
iris.preds = predict(object=iris.net,newdata=iris[-samp,],type="class")

print(table(iris$Species[-samp],iris.preds))