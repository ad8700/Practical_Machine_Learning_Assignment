library(caret)
library(ggplot2)
data(iris)

inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modLda <- train(Species ~., data=training, method="lda")
modNb <- train(Species ~., data=training, method="nb")
plda <- predict(modLda, testing)
pnb <- predict(modNb, testing)
table(plda, pnb)

#graphing comparison of results
equalPredictions =(plda==pnb)
qplot(Petal.Width, Petal.Length, colour=equalPredictions, data=testing)



