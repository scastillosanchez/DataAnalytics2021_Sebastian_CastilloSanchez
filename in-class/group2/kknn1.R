#Sebastian Castillo-Sanchez
#kknn1

#bring in data
require(kknn)
data(iris)

#split data into learn/validation
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
	prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]

#conduct kknn
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
	kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn)
table(iris.valid$Species, fit) #view table of the fit of the species
pcol <- as.character(as.numeric(iris.valid$Species))

#plot pairplot
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")[(iris.valid$Species != fit)+1])

