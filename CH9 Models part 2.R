#A nearest-neighbor classifier based on the Euclidean distance is implemented in the
#package class in R.

permut <- sample(1:nrow(iris), nrow(iris), replace = F)

#Then we define this permutation as an ordering in which the records of our data set
#should be ordered and store the shuffled data set in the object iris.shuffled:
ord <- order(permut)
iris.shuffled <- iris[ord,]

#Now define how large the fraction for the training set should be-here 2/3-and
#take the first two thirds of the data set as a training set and the last third as a test set:
prop.train <- 2/3
k <- round(prop.train*nrow(iris))
iris.training <- iris.shuffled[1:k,]
iris.test <- iris.shuffled[(k+1):nrow(iris),]

library(class)
#KNN
iris.knn <- knn(iris.training[,1:4],iris.test[,1:4],
                iris.training[,5],k=3)
table(iris.knn,iris.test[,5])

#ANN
#For the example of multilayer perceptrons in R, we use the same training and test
#data as for the nearest-neighbor classifier above. The multilayer perceptron can only
#process numerical values. Therefore, we first have to transform the categorical attribute
#Species into a numerical attribute:

x <- iris.training
x$Species <- as.numeric(x$Species)

#The multilayer perceptron is constructed and trained in the following way, where
#the library neuralnet needs to be installed first:

#In principle, a multilayer perceptron may have any number of
#hidden layers, but it is most common to use only a single hidden layer, based on
#certain theoretical results about the capabilities of such neural networks

#the number of input nd output neurons is fixed by the data analysis task.the
#only choices left for a multilayer perceptron are the number of hidden layers and
#the number of neurons in these layers.

#A simple rule of thumb, which often leads to acceptable training results, is to use
#1/2(#inputs+#outputs) hidden neurons, where #inputs and #outputs are the numbers
#of input and output attributes, respectively.

#However, even though a wrong number
#of hidden neurons, especially if it is chosen too small, can lead to bad results, one has
#to concede that other factors, especially the choice and scaling of the input attributes,
#are much more important for the success of neural network model building.
library(neuralnet)
iris.nn <- neuralnet(x$Species + x$Sepal.Length ~
               x$Sepal.Width + x$Petal.Length
               + x$Petal.Width, x,
               hidden=c(3))
#The first argument of neuralnet defines that the attributes species and sepal
#length correspond to the output neurons. The other three attributes correspond to
#the input neurons. x specifies the training data set. The parameter hidden defines
#how many hidden layers the multilayer perceptron should have and how many neurons
#in each hidden layer should be. In the above example, there is only one hidden
#layer with three neurons. When we replace c(3) by c(4,2), there would be two
#hidden layers, one with four and one with two neurons.

#The training of the multilayer perceptron can take some time, especially for larger
#data sets.
plot(iris.nn)

#The output of the multilayer perceptron for the test set can be calculated in the
#following way. Note that we first have to remove the output attributes from the test
#set:

y <- iris.test
y <- y[-5]
y <- y[-1]
y.out <- compute(iris.nn,y)

#We can then compare the target outputs for the training set with the outputs from
#the multilayer perceptron. If we want to compute the squared errors for the second
#output neuron-the sepal length-we can do this in the following way:

y.sqerr <- (y[1] - y.out$net.result[,2])^2


#SVM

#A support vector machine topredict the species in the Iris data set 
#based on the other attributes can be constructed in the following way.

library(e1071)
iris.svm <- svm(Species ~ ., data = iris.training)
table(predict(iris.svm,iris.test[1:4]),iris.test[,5])

#The last line prints the confusion matrix for the test data set.

#The function svm works also for support vector regression. 
#We could, for instance, use

iris.svm <- svm(Petal.Width ~ ., data = iris.training)
sqerr <- (predict(iris.svm,iris.test[-4])
            -iris.test[4])^2
#to predict the numerical attribute petal width based on the other attributes and to
#compute the squared errors for the test set.

#Random FOrest

#As an example for ensemble methods, we consider random forest with the training
#and test data of the Iris data set as before.

library(randomForest)
iris.rf <- randomForest(Species ~., iris.training)
table(predict(iris.rf,iris.test[1:4]),iris.test[,5])

#In this way, a random forest is constructed to predict the species in the Iris data set
#based on the other attributes. The last line of the code prints the confusion matrix
#for the test data set.