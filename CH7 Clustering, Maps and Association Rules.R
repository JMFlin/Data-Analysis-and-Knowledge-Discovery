#Clusters

iris.norm <- iris
iris.num <- iris.norm[1:4]
iris.cl <- hclust(dist(iris.num), method="ward.D")
plot(iris.cl)

#Here, the Ward method for the cluster distance aggregation function as described.
#For the other cluster distance aggregation functions in the
#table, one simply has to replace ward by single (for single linkage), by complete
#(for complete linkage), by average (for average linkage), or by centroid.

#For heatmaps, the library gplots is required that needs installing first:
library(gplots)
rowv <- as.dendrogram(hclust(dist(iris.num),method="ward.D"))
colv <- as.dendrogram(hclust(dist(t(iris.num)),method="ward.D"))
heatmap.2(as.matrix(iris.num), Rowv=rowv,Colv=colv,trace="none")

#The R-function kmeans carries out k-means clustering.
iris.km <- kmeans(iris.num,centers=3)
#The desired numbers of clusters is specified by the parameter centers. The location
#of the cluster centers and the assignment of the data to the clusters is obtained
#by the print-function:
print(iris.km)
#For fuzzy c-means clustering, the library cluster is required. The clustering
#is carried out by the method fanny similar to kmeans:
library(cluster)
iris.fcm <- fanny(iris.num,3)
iris.fcm
#The last line provides the necessary information on the clustering results, especially
#the membership degrees to the clusters.

#Gaussian mixture decomposition automatically determining the number of clusters
#requires the library mlcust to be installed first:
library(mclust)
iris.em <- mclustBIC(iris.num[,1:4])
iris.mod <- mclustModel(iris.num[,1:4],iris.em)
summary(iris.mod)
#The last line lists the assignment of the data to the clusters.

#Density-based clustering with DBSCAN is implemented in the library fpc
#which needs installation first:
library(fpc)
iris.dbscan <- dbscan(iris.num[,1:4],1.0,showplot=T)
iris.dbscan$cluster
#The last line will print out the assignment of the data to the clusters. Singletons or
#outliers are marked by the number zero. The second argument in dbscan (in the
#above example 1.0) is the parameter ?? for DBSCAN. showplot=T will generate
#a plot of the clustering result projected to the first two dimensions of the data set.

#Self-organizing maps

#The library som provides methods for self organizing maps.
library(som)
iris.som <- som(iris.num,xdim=5,ydim=5)
plot(iris.som)
#xdim and ydim define the number of nodes in the mesh in x- and y-directions,
#respectively. plot will show, for each node in the mesh, a representation of the
#values in the form of parallel coordinates.

#Association Rules

#For association rule mining, the library arules is required in which the function
#apriori is defined. This library does not come along with R directly and needs to
#be installed first.
#Here we use an artificial data set basket that we enter manually. The data set
#is a list of vectors where each vector contains the items that were bought:
library(arules)
baskets <- list(c("a","b","c"), c("a","d","e"),
                 c("b","c","d"), c("a","b","c","d"),
                 c("b","c"), c("a","b","d"),
                 c("d","e"), c("a","b","c","d"),
                 c("c","d","e"), c("a","b","c"))
rules <- apriori(baskets,parameter = list(supp=0.1,
                                            conf=0.8,
                                            target="rules"))
inspect(rules)
#The last command lists the rules with their support, confidence, and lift.