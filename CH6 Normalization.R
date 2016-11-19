#Normalization and standardization of numeric attributes can be achieved in the following
#way. The function is.factor returns true if the corresponding attribute is
#categorical (or ordinal), so that we can ensure with this function that normalization
#is only applied to all numerical attributes, but not to the categorical ones. With the
#following R-code, z-score standardization is applied to all numerical attributes:
iris.norm <- iris
for (i in c(1:length(iris.norm))){
    if (!is.factor(iris.norm[,i])){
        attr.mean <- mean(iris.norm[,i])
        attr.sd <- sd(iris.norm[,i])
        iris.norm[,i] <- (iris.norm[,i]-attr.mean)/attr.sd
    }
}
#Other normalization and standardization techniques can carried out in a similar
#manner. Of course, instead of the functions mean (for the mean value) and sd (for
#the standard deviation), other functions like min (for the minimum), max (for the
#maximum), median (for the median), or IQR (for the interquartile range) have to be used.