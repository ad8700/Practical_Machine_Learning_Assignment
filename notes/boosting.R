library(ISLR)
library(ggplot2)
library(caret)
data(Wage)

Wage <- subset(Wage, select=-c(logWage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage ~., method="gbm", data=training, verbose=FALSE)
print(modFit)

#plotting
qplot(predict(modFit, testing), wage, data=testing)



