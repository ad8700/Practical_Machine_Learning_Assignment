library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(Y=spam$type,
                               p=0.7, list=FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8, arr.ind=T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

#rotate the plot
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

#PCA in R - prcomp

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])
prComp$rotation

#PCA on Spam Data
typeColor <- ((spam$type="spam")*1+1) #Note to self, this didn't work
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2], col=typeColor, xlab="PC1",ylab="PC2")

#PCA with caret

preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

preProc <- preProcess(log10(training[,-58]+1), method="pca", pcaComp = 2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~., method="glm", data=trainPC)

#In the test data, have to use the same Principal Components

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))

#Preprocessing as part of training process
modelFit <- train(training$type~., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

