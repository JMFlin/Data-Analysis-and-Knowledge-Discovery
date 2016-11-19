library(ggplot2)
library(RCurl)
library(boot)
library(caret)
library(car)
library(FNN)

fileURL <- getURL("archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv")
my_data <- read.csv(text = fileURL, sep = ";", header = T)

str(my_data)
head(my_data)
table(my_data$quality)
describe(my_data)

#---------------------------Linear Regression

#Scaling the data
my_data$quality <- as.numeric(my_data$quality)
my_scale <- scale(my_data[,1:11], scale = TRUE, center = TRUE)
my_scale <- data.frame(my_scale)
as <- cbind(my_scale, my_data[,12])


#train and a test set
index <- sample(1:nrow(as), round(0.7*nrow(as)))
train <- as[index,]
test <- as[-index,]
names(train) <- c("fixed.acidity", "volatile.acidity",	"citric.acid",	"residual.sugar", "chlorides", "free.sulfur.dioxide",	"total.sulfur.dioxide", "density",	"pH",	"sulphates" , "alcohol", "quality")
names(test) <- c("fixed.acidity", "volatile.acidity",    "citric.acid",	"residual.sugar", "chlorides", "free.sulfur.dioxide",	"total.sulfur.dioxide", "density",	"pH",	"sulphates" , "alcohol", "quality")
names(as) <- c("fixed.acidity", "volatile.acidity",    "citric.acid",	"residual.sugar", "chlorides", "free.sulfur.dioxide",	"total.sulfur.dioxide", "density",	"pH",	"sulphates" , "alcohol", "quality")
train$quality <- as.numeric(train$quality)
test$quality <- as.numeric(test$quality)
as$quality <- as.numeric(as$quality)

#Linear model
lm.fit <- lm(quality~.-density,data = train)
summary(lm.fit)#Some high p-values
predict.lm <- predict(lm.fit,test)
round(confint(lm.fit),3)
MSE.lm <- sum((predict.lm - test$quality)^2)/nrow(test)#Since we are dealing with a regression problem, 
#we are going to use the mean squared error (MSE) as a measure of how much our predictions are far away from the real data.
#CV errors for the linear models:
lm.fit <- glm(quality~.,data=my_data)
cv.glm(my_data,lm.fit,K=10)$delta[1]

#KNN

train.X <- train[,1:11] 
#train.Y <- train[,12]

test.X <- test[,1:11] 
#test.Y <- test[,12]
train.class <- train$quality

errors1 <- as.matrix(rep(NA ,10))
errors2 <- as.matrix(rep(NA ,10))
errors3 <- as.matrix(rep(NA ,10))
errors4 <- as.matrix(rep(NA ,10))
errors5 <- as.matrix(rep(NA ,10))
errors6 <- as.matrix(rep(NA ,10))
errors7 <- as.matrix(rep(NA ,10))
errors8 <- as.matrix(rep(NA ,10))
errors9 <- as.matrix(rep(NA ,10))
errors10 <- as.matrix(rep(NA ,10))
errors1.1 <- cbind(errors1, errors2, errors3, errors4, errors5, errors6, errors7, errors8, errors9, errors10)
set.seed(79428)
for(i in 1:10){
    #index <- sample(1:nrow(as),round(0.9*nrow(as)))
    #train.X <- as[index,]
    #test.X <- as[-index,]
    for(j in 1:10){
        knn.pred <- knn.reg(train.X, test.X, train.class, k=j)
        errors1.1[j,i] <- sum((knn.pred$pred - test$quality)^2)/nrow(test)
    }
}#9 neighbors is the smallest one.

#MSE.knn <- apply(errors1.1, 2,mean)
#plot(MSE.knn, type = "b")
#points (x = which.min(MSE.knn), y = MSE.knn[which.min(MSE.knn)], col ="red",cex=2,pch =20)
#which.min(MSE.knn)

#knn.pred <- knn(train.X, test.X, train.class, k=8)
#pred.table <- table(knn.pred, test$quality)
#sum(diag(pred.table))/sum(pred.table)
#pred.table
