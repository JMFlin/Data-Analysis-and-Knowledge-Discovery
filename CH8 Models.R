#Classification

library(rpart)
iris.train <- c(sample(1:150,75))
iris.dtree <- rpart(Species~.,data=iris,
                      subset=iris.train)
library(rattle)
drawTreeNodes(iris.dtree)
table(predict(iris.dtree,iris[-iris.train,],
                type="class"),iris[-iris.train,"Species"])

#Naive Bayes classifiers use normal distributions by default for numerical attributes.
#The package e1071 must be installed first:
library(e1071)
iris.train <- c(sample(1:150,75))
iris.nbayes <- naiveBayes(Species~.,data=iris,
                    subset=iris.train)
table(predict(iris.nbayes,iris[-iris.train,],
            type="class"),
    iris[-iris.train,"Species"])
#As in the example of the decision tree, the Iris data set is split into a training and
#a test data set, and the confusion matrix is printed. The parameters for the normal
#distributions of the classes can be obtained in the following way:
print(iris.nbayes)
#If Laplace correction should be applied for categorical attribute, this can be
#achieved by setting the parameter laplace to the desired value when calling the
#function naiveBayes

#Regression

#The default method is based on Huber's error function. If Tukey's biweight should
#be used, the parameter method should be changed in the following way:
iris.rlm <- rlm(iris$Petal.Width ~ iris$Sepal.Length
              + iris$Sepal.Width + iris$Petal.Length,
              method="MM")
#A plot of the computed weights can be obtained by the following command:
plot(iris.rlm$w)