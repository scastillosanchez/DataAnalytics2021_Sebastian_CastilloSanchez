#svm lab 3
#sebastian castillo-sanchez
data(iris)
attach(iris)

model <- svm(Species ~ ., data = iris)

x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

pred <- predict(model, x) # or pred <- fitted(model)

table(pred, y)

pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1]) #plot species

x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

m   <- svm(x, y)
new <- predict(m, x)

plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

m <- svm(X, gamma = 0.1)
#m <- svm(~., data = X, gamma = 0.1) # or  m <- svm(~ a + b, gamma = 0.1)

newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

#plot using svm index
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

#make weights and svm for iris
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)