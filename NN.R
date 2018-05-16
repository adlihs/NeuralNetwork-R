## CREATTING INDEX VARIABLE

#READ THE DATA
data <- read.csv("cereals.csv", header = T)

#RANDOM SAMPLING
samplesize <- 0.60 * nrow(data)
set.seed(80)
index <- sample(seq_len(nrow(data)), size = samplesize)

#CREATE TRAINING AND TEST SET
datatrain <- data[index,]
datatest <- data[-index,]


##SCALE DATA FOR NEURAL NETWORK
max <- apply(data, 2, max)
min <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = min, scale = max - min))


##FIT NERAL NETWORK

#Install package
install.packages("neuralnet")


#Load pacakge
library(neuralnet)


#Creating training and test set
trainNN <- scaled[index,]
testNN <- scaled[-index,]

#Fit neural network
set.seed(2)

NN <- neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 1000, linear.output = T)

#plot neural network
plot(NN)


##PREDICTION USING NEURAL NETWORK
predict_testNN <- compute(NN, testNN[,c(1:5)])
predict_testNN <- (predict_testNN$net.result * (max(data$rating))) + min(data$rating)

plot(datatest$rating, predict_testNN, col = 'blue', pch=16, ylab = "predicted rating NN",xlab = "real rating")

abline(0,1)

#Calculate root mean square error (RMSE)
RMSE.NN <- (sum((datatest$rating - predict_testNN)^2) / nrow(datatest)) ^ 0.5

## CROSS VALIDATION OF NEURAL MODEL

# install relevant libraries
install.packages("boot")
install.packages("plyr")

#load libraries
library(boot)
library(plyr)


#Initiative variables
set.seed(50)
k <- 100
RMSE.NN <- NULL


List <- list()

#Fit neural network model within nested for loop

for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}
    
Matrix.RMSE = do.call(cbind, List)