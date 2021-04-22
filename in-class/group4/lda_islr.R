#lda lab from book

library(MASS)
library(ISLR)

help(lda)


data("Smarket")
attach(Smarket)
head(Smarket)
names(Smarket)
str(Smarket)
dim(Smarket)

train <- (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]


lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit


lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)
