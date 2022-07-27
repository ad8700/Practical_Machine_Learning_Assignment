library(ggplot2)
library(caret)
data(iris)
names(iris)


table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

#plotting
qplot(Petal.Width, Sepal.Width, data=training, color=Species)

#modeling
modFit <- train(Species ~., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=0.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

#predict new values
predict(modFit, newdata=testing)


