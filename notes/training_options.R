library(caret)
library(kernlab)
data(spam)

inTrain <- caret::createDataPartition(y=spam$type,
                                      p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~., data=training, method="glm")

#setting seed
set.seed(1235)
modelFit2 <- train(type ~., data=training, method="glm")
modelFit2

set.seed(1235)
modelFit3 <- train(type ~., data=training, method="glm")
modelFit3


