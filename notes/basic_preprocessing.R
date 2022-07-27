library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve, main="", xlab="ave. capital run length")


#Standardizing variables
mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

#Standardizing on test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve) 
#must use the mean and sd from the training set to standardize the testing set value
mean(testCapAveS)
sd(testCapAveS)

#using preprocess function on training set
preObj <- preProcess(training[,-58], method=c("center", "scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

#using preprocess function on test set
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)

#passing preprocessing directly as an arg to caret::train
set.seed(32343)
modelFit <- train(type ~., data=training,
                  preProcess=c("center","scale"), method="glm")
modelFit

#Standardizing box-cox transformations
preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

#Imputing data
set.seed(13343)
##making some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.5)=1
training$capAve[selectNA] <- NA
##Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve
#standardize True values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)


