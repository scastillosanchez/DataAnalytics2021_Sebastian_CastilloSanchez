#Sebastian Castillo-Sanchez
#KMeans in-class

library(ISLR)
library(ggplot2)
library(cluster)

#view iris data/info
head(iris)
str(iris)

#view the relationship between petal length and width with species too
plot1 <- ggplot(iris, aes(Petal.Length,Petal.Width, color=Species))
print(plot1 + geom_point(size=3))

#set random number seed
set.seed(101)

irisClusters <- kmeans(iris[,1:4], 3, nstart = 20) # nstart is the number of random start
print(irisClusters)
table(irisClusters$cluster,iris$Species) #view clusters in regard to species


# plotting the clusters
clusplot(iris,irisClusters$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)

