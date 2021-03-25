#randomForest lab
#Sebastian Castillo-Sanchez

#load in kyphosis data and do random forest model
require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF)
importance(fitKF) 

#random forest model for swiss dat and view results/importance
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) 
importance(fitSwiss)

#plotting importance and model
varImpPlot(fitSwiss)
plot(fitSwiss)
getTree(fitSwiss,1, labelVar=TRUE)

#look at documentation and other data
help(randomForest)
help(rfcv)
data(imports85)