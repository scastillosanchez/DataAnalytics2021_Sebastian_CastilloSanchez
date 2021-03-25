#rpart lab all 4 parts
#Sebastian Castillo-Sanchez

#part 1
require(rpart)
require(rpart.plot)
#use swiss data
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) #plot the rpart plot
text(swiss_rpart) #text of the rpart plot
rpart.plot(swiss_rpart) #using a specific package to plot rpart

#part 2 
#anova tree using cu.summary data
fitM <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)

#viewing important info of tree like cv
printcp(fitM) 
plotcp(fitM)
summary(fitM)
par(mfrow=c(1,2)) 
rsq.rpart(fitM)

# plotting the tree with text
plot(fitM, uniform=TRUE, main="Regression Tree for Mileage ")
text(fitM, use.n=TRUE, all=TRUE, cex=.8)

#performing tree pruning
pfitM<- prune(fitM, cp=0.01160389)

# plot the pruning
plot(pfitM, uniform=TRUE, main="Pruned Regression Tree for Mileage")
text(pfitM, use.n=TRUE, all=TRUE, cex=.8)


#part 3
library(e1071)
library(rpart)
#load in glass dataset
data(Glass, package="mlbench")
#split data into train and test
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]
#do rpart model and predict on test set
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")
printcp(rpart.model)
plotcp(rpart.model)

#viewing rpart model
rsq.rpart(rpart.model)
print(rpart.model)

#plotting the rpart model and predictions
plot(rpart.model,compress=TRUE)
text(rpart.model, use.n=TRUE)
plot(rpart.pred)


#part 4
#using kyphosis data
fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
#view summary and plot of results
printcp(fitK) 
plotcp(fitK) 
summary(fitK) 
#view plot of rpart tree
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)

#prune tree model and then plot
pfitK<- prune(fitK, cp=   fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])
plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.8)
