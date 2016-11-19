library(ggplot2)
library(RCurl)
library(GGally)
library(grid)
library(gridExtra)
library(car)
library(psych)
library(corrplot)
library(ggfortify)

#Remember to widen your plot screen before exporting pics

fileURL <- getURL("archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv")
my_data <- read.csv(text = fileURL, sep = ";", header = T)

str(my_data)
head(my_data)
table(my_data$quality)
describe(my_data)

#-------------------1.Plot histograms of the attributes. 
#To determine the number of bins use at least three different methods 
#introduced in the lectures (Sturges' rule, Scott's rule, square-choice, Freedman-Diaconis rule). Compare the results.

for(i in 1:12){
    breaks <- pretty(range(my_data[,i]), n = nclass.Sturges(my_data[,i]), min.n = 1)
    bwidth <- breaks[2]-breaks[1]
    df <- data.frame(my_data[,i])
    a <- ggplot(df,aes(my_data[,i]))+geom_histogram(binwidth=bwidth,fill="white",colour="black")+
        labs(title = paste(names(my_data)[i],"- Sturges' Method"))+
     theme(plot.title = element_text(size = rel(1.5)))
    

    breaks <- pretty(range(my_data[,i]), n = nclass.scott(my_data[,i]), min.n = 1)
    bwidth <- breaks[2]-breaks[1]
    df <- data.frame(my_data[,i])
    b <- ggplot(df,aes(my_data[,i]))+geom_histogram(binwidth=bwidth,fill="white",colour="black")+
        labs(title = paste(names(my_data)[i],"- Scott's Method"))+
        theme(plot.title = element_text(size = rel(1.5)))
    
    
    breaks <- pretty(range(my_data[,i]), n = nclass.FD(my_data[,i]), min.n = 1)
    bwidth <- breaks[2]-breaks[1]
    df <- data.frame(my_data[,i])
    c <- ggplot(df,aes(my_data[,i]))+geom_histogram(binwidth=bwidth,fill="white",colour="black")+
        labs(title = paste(names(my_data)[i],"- Freedman-Diaconis' Method"))+
        theme(plot.title = element_text(size = rel(1.5)))
    
    grid.arrange(a, b, c, ncol=1)
}

#-------------------2. Produce scatter plot of the data and the parallel coordinates representation.

pairs(my_data, gap=0, pch=19, cex=0.4, col="darkblue", panel = "panel.smooth")
title(sub="Scatterplot of Chemical Attributes", cex=0.8)
#by plotting the variables against each other it becomes obvious 
#that some are strongly correlated: in other words, there is an 
#overlap in the power of some variables at explaining/accounting for 
#the data variability. A PCA will help disentangling these correlations.

my_data[,12] <- as.factor(my_data[,12])

pcp <- ggparcoord(data = my_data, columns = 1:11, groupColumn = 12,
                  showPoints = TRUE, title = "Parallel Coordinate Plot for Wine Data", scale = "std")
pcp <- pcp + theme(plot.title = element_text(size = rel(1.5)))
pcp #Can't separate the classes but I see some outliers.

for(m in 1){
    my_data[,12] <- as.numeric(my_data[,12])
    plot_list = list()
    k <- 1
    for(i in 1:6){
        for(j in (i+1):7){
            b <- my_data[which(my_data$quality == i | my_data$quality == j),]
            b[,12] <- as.factor(b[,12])
            names(b) <- c("fixed", "volatile", "citric", "resid.s", "chlorides",
                          "free", "total", "density", "pH", "sulph", "alcohol", "quality")
            pcp <- ggparcoord(data = b, columns = 1:11, groupColumn = 12,
              showPoints = TRUE, scale = "std") + labs(x = NULL, y = NULL)
            plot_list[[k]] <- pcp
            k <- k+1
        }
    }
    do.call("grid.arrange", c(plot_list, ncol=3))#21 plots
}

#-------------------3. Principal component analysis (PCA) with and without normalization. 
#Project the data to the first two principal components. What can be observed?

#I first briefly examine the data. I notice that the variables have vastly different means.
round(apply(my_data, 2, mean),3)
round(apply(my_data, 2, var),3)#Not surprisingly, the variables also have vastly different variances
#Thus, it is important to standardize the
#variables to have mean zero and standard deviation one before performing PCA.

#scale=T standardizes the variables to the same relative scale, 
#avoiding some variables to become dominant just because of their large measurement units.

pca1 <- prcomp(my_data[,1:11], scale = FALSE, center = TRUE)
pca2 <- prcomp(my_data[,1:11], scale = TRUE, center = TRUE)

pr.var1 <- pca1$sdev^2
pve1 <- pr.var1/sum(pr.var1)

pr.var2 <- pca2$sdev^2
pve2 <- pr.var2/sum(pr.var2)

summary(pca1)
summary(pca2)
#the summary indicates that four PCs where created: the number 
#of possible PCs always equals the number of original variables.

#PC1 and PC2 explain respectively ~30% and ~14% of the data's 
#total variability, summing up to a 44% of the total variability. 

#a "scree plot" allows a graphical assessment of the relative 
#contribution of the PCs in explaining the variability of the data.

#pca2$roation # the "Rotation" matrix contains the "loadings" of each 
#of the original variables on the newly created PCs.

variances <- data.frame(variances=pca1$sdev**2, pcomp=1:length(pca1$sdev))
varPlot <- ggplot(variances, aes(pcomp, variances))+ geom_bar(stat="identity", fill="gray") + geom_line() + 
    labs(title = "Scree plot without normalization")+
    theme(plot.title = element_text(size = rel(1.5)))
varPlot#Ylab is Eigenvalues! Xlab is Eigenvectors
par(mfrow = c(1,1))
plot(cumsum(pve1), main = "Not Normalized", xlab="Principal  Component", 
     ylab="Cumulative  Proportion  of Variance  Explained", ylim=c(0,1),type="b")#Xlab is Eigenvectors

variances <- data.frame(variances=pca2$sdev**2, pcomp=1:length(pca2$sdev))
varPlot <- ggplot(variances, aes(pcomp, variances))+ geom_bar(stat="identity", fill="gray") + geom_line() + 
    labs(title = "Scree plot with normalization")+
    theme(plot.title = element_text(size = rel(1.5)))
varPlot#Ylab is Eigenvalues! Xlab is Eigenvectors
par(mfrow = c(1,1))
plot(cumsum(pve2), main = "Normalized", xlab="Principal  Component", 
     ylab="Cumulative  Proportion  of Variance  Explained", ylim=c(0,1),type="b")#Xlab is Eigenvectors


my_data[,12] <- as.numeric(my_data[,12])

PCbiplot <- function(PC, x="PC1", y="PC2", mm) {
    # PC being a prcomp object
    # data <- data.frame(obsnames=row.names(PC$x), PC$x)
    data <- data.frame(obsnames="", PC$x)#data.frame(obsnames=my_data[,12], PC$x)
    plot <- ggplot(data, aes_string(x=x, y=y))# + geom_text(alpha=.4, size=3, aes(label=obsnames))
    #geom_text is redundant here because we have obsnames as blank
    plot <- plot + geom_hline(aes(0), size=.2, yintercept = 0) + geom_vline(aes(0), size=.2, xintercept = 0) + geom_point(alpha = 1,size = 2)
    datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
    mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
    )
    datapc <- transform(datapc,
                        v1 = .7 * mult * (get(x)),
                        v2 = .7 * mult * (get(y))
    )
    plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 6.5, vjust=1, color="red")
    plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")+
        labs(title = mm)+
        theme(plot.title = element_text(size = rel(1.5)))
    plot
}
fit.1 <- prcomp(my_data[,1:12], scale=TRUE, center = TRUE)#Zero mean and unit variance
fit.2 <- prcomp(my_data[,1:12], scale=FALSE, center = TRUE)

PCbiplot(fit.1, mm = "PCA with normalization")
PCbiplot(fit.2, mm = "PCA without normalization")

#-------------------4.Produce 2D MDS representation for the data. Compare the result to PCA.

#http://rpubs.com/sinhrks/plot_mds

#Standardize the data first

a <- scale(my_data, scale = TRUE, center = TRUE)

d <- dist(a, method = "euclidean") # euclidean distances between the rows
fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
plot <- autoplot(fit, xlab="Coordinate 1", ylab="Coordinate 2", main = "Metric MDS")
plot <- plot +theme(plot.title = element_text(size = rel(1.5))) +geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
plot

#-------------------5. Calculate the Spearson and Kendall's tau correlation tables for the attributes.

cor(my_data[,1:11], use="complete.obs", method="kendall") 
cor(my_data[,1:11], use="complete.obs", method="pearson") 

#See Rcolor pdf
#Remember to widen your plot screen before exporting
corrplot(cor(my_data[,1:11], use="complete.obs", method="kendall"), method = "number", 
         col = colorRampPalette(c("red","lightgray","blue"))(100), tl.col = "black", 
         mar=c(0,0,1,0), title = "Kendall's Tau Correlation", tl.cex=0.8)

corrplot(cor(my_data[,1:11], use="complete.obs", method="pearson"), method = "number", 
         col = colorRampPalette(c("red","lightgray","blue"))(100), tl.col = "black", 
         mar=c(0,0,1,0), title = "Pearson Correlation", ,tl.cex=0.8)
