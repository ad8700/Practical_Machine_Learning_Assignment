library(caret)
library(kernlab)
data(spam)

inTrain <- caret::createDataPartition(y=spam$type,
                                      p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32323)
folds <- createFolds(y=spam$type, k=10,
                     list=TRUE, returnTrain=TRUE)
sapply(folds, length)

folds[[1]][1:10]

#To return test set samples
folds <- createFolds(y=spam$type, k=10,
                     list=TRUE, returnTrain=FALSE)
sapply(folds, length)

folds[[1]][1:10]

#To resample
folds <- createResample(y=spam$type, times=10,
                        list=TRUE)
sapply(folds,length)

folds[[1]][1:10]


#to create time slices
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20,
                          horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]