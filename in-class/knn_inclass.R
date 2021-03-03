#Sebastian Castillo-Sanchez
#KNN in class
# KNN example using ISLR package (Textbook) 

#load in libraries
library(ISLR) 
library(class)
library(ggplot2)

#look at caravan data
head(Caravan)
str(Caravan)

#check dimensions of data and summary
dim(Caravan)  
summary(Caravan)
summary(Caravan$Purchase)

any(is.na(Caravan)) #check for null values

var(Caravan[,1]) # Variance of the 1st column is 165.0378
var(Caravan[,2]) # Variance of the 2nd column is 0.1647
var(Caravan[,3]) # Variance of the 3rd column is 0.6238

#extract purchase column
purchase <- Caravan[,86]
purchase

#standardize every column but the purchase one
StandardizedCaravan <- scale(Caravan[,-86]) # when we use -86 it will not include the 86th column.
var(StandardizedCaravan[,1])
var(StandardizedCaravan[,2])
var(StandardizedCaravan[,3])

# test set
test_index <- 1:1000
test_data <- StandardizedCaravan[test_index,]
test_purchase <- purchase[test_index]

# train set
train_data <- StandardizedCaravan[-test_index,]
train_purchase <- purchase[-test_index]

# set seed
set.seed(101)
predicted_purchase <- knn(train_data,test_data,train_purchase, k = 3) #knn prediction
head(predicted_purchase)

missClassError <- mean(test_purchase != predicted_purchase) #check error
print(missClassError)

table(predicted_purchase, test_purchase)

# Choosing the K value
# we can write a for-loop
predicted_purchase <- NULL
error_rate <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted_purchase <- knn(train_data, test_data, train_purchase, k =i) #knn prediction for different k values
  error_rate[i] <- mean(test_purchase != predicted_purchase)
}

print(error_rate)

#plot error rate with k value 
k_values <- 1:20

error_df <- data.frame(error_rate, k_values)
print(error_df)
ggplot(error_df,aes(k_values,error_rate)) + geom_point() + geom_line(lty='dotted', color='blue')


#trying the logistic regression to compare as seen in the ISLR book
glm.fit <- glm(Purchase~., data = Caravan, subset = -test_index, family = "binomial")
glm.prob <- predict(glm.fit, Caravan[test_index, ], type="response")
glm.pred=rep ("No" ,1000)
glm.pred[glm.prob >.5]="Yes"
table(glm.pred ,test_purchase)
