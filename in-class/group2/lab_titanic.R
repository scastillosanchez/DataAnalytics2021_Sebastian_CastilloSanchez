#Sebastian Castillo-Sanchez
#titanic lab

#load in data
library(rpart)
library(party)
library(stats)
library(titanic)
library(rpart.plot)

#Titanic doesn't work?
#data("Titanic")
#head(Titanic)
head(titanic_train)

#rpart model
titanic_train_sub <- subset(titanic_train, select = -c(Name, PassengerId, Ticket, Cabin))
rpartmodel <- rpart(Survived~., titanic_train_sub, method = "class")
rpartmodel
rpart.plot(rpartmodel)

#ctree
#remove character columns
titanic_train_ctree <- subset(titanic_train, select = -c(Name, PassengerId, Ticket, Cabin, Sex, Embarked))
titanic_ctree <- ctree(Survived ~., data = titanic_train_ctree)
plot(titanic_ctree)


#hclust
sample <- sample(1:dim(titanic_train_ctree)[1], 30)
sampled <- titanic_train_ctree[sample,]
sampled$Survived <- NULL
#par(mar=rep(0.2, 4))
#heatmap(data.matrix(titanic_train_ctree))
hh <- hclust(dist(sampled), method="ave")
plot(hh, hang=-1, labels=titanic_train_ctree$Survived[sample])

