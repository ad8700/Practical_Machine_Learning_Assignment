library(ISLR)
library(ggplot2)
library(caret)
data(Wage)

Wage <- subset(Wage, select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

dim(training)
dim(testing)

#plotting features
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")

qplot(age, wage, data=training)

qplot(age, wage, data=training, color=jobclass)

qplot(age, wage, data=training, color=education)

#fit a linear model
modFit <- train(wage ~ age + jobclass + education, 
                method="lm", data=training)
finMod <- modFit$finalModel
print(modFit)

#diagnostic plots
plot(finMod, 1, pch=19, cex=0.5, col="#00000010")


#plot by index
plot(finMod$residuals, pch=19)

#predicting vs truth
pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)


#Using all covariates
modFitAll <- train(wage ~ ., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)