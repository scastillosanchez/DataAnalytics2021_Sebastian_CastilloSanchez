#Sebastian Castillo-Sanchez
#lpad in data
library(randomForest)
data1 <- read.csv(file.choose(), header = TRUE)
head(data1)

#look at data
colnames(data1) <- c("Buying Price", "Maintenance", "NumDoors", "NumPersons",
                     "BootSpace", "Safety", "Condition")
head(data1)
str(data1)

#view levels of the condition variable
levels(data1$Condition)
summary(data1)

#create train and validation sets
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

#create first random forest model
model1 <- randomForest(Condition~., data=TrainSet, importance=TRUE)
model1

#create second random forest model
model2 <- randomForest(Condition~., data=TrainSet, importance=TRUE, ntree=500, 
                       mtry=6)
model2

#perform predictions for training set
predTrain <- predict(model2, TrainSet, type = "class")
table(predTrain, TrainSet$Condition)

#perform predictions for validation set
predValid <- predict(model2, ValidSet, type="class")
table(predValid, ValidSet$Condition)

#view importance of the second random forest model
importance(model2)
varImpPlot(model2)

#create random forest model for different number of features mtry=i
a = c()
i = 5 
for (i in 3:8) {
  model3 <- randomForest(Condition~., data=TrainSet, importance=TRUE, ntree=500, 
                         mtry=i)
  predValid <- predict(model3, ValidSet, type="class")
  a[i-2] = mean(predValid == ValidSet$Condition)
}
#view and plot different models
a
plot(3:8, a)

#load in libraries
library(rpart)
library(caret)
library(e1071)

#create a rpart model and predict on train set
model_dt <- train(Condition~., data=TrainSet, method="rpart")
model_dt_1 <- predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1, TrainSet$Condition) #view mean of model

#create a rpart model and predict on validation set
model_dt_vs <- predict(model_dt, newdata=ValidSet)
table(model_dt_vs, ValidSet$Condition)
mean(model_dt_vs, ValidSet$Condition) #view mean of model

