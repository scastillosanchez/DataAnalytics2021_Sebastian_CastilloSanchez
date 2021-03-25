#rpart abalone

require(rpart)
require(rpart.plot)

#read in abalone data
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE,
                    sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight',
                       'rings' )
abalone$rings <- as.numeric(abalone$rings)
#abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
#abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)

#rpart on rings
abalone_rpart <- rpart(rings ~ ., data=abalone)
rpart.plot(abalone_rpart)