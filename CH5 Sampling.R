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
