#Sebastian Castillo-Sanchez

#load in libraries
library(ISLR)
library(MASS)
library(boot)
set.seed(1)

??cv.glm #read glm documentation
help("sample") #read sample documentation

#set training sample
train = sample(392,196)

#create linear regression on auto data using the train subset
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)

attach(Auto) #attaching auto data for easy use
mean((mpg-predict(lm.fit,Auto))[-train]^2) #calculating MSE on validation set 

#quadratic regression and MSE
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#cubic regression and MSE
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


#set different seed for different train subset
set.seed(2)
train = sample(392,196)

#first regression
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

#quadratic regression and MSE
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) 
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#cubic regression and MSE
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#k-fold cross-validation
??cv.glm
set.seed(17)
help("rep") #view rep documentation
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1]
}
cv.error.10 #view cv error