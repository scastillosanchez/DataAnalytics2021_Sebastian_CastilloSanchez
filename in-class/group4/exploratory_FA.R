#exploratory factor analysis
#from course repository

data <- read.csv("dataset_exploratoryFactorAnalysis.csv")
#data <- read.fwf("example_exploratoryFactorAnalysis.txt")
data


install.packages("psych")
library(psych)
corMat  <- cor(data)
corMat


solution <- fa(r = corMat, nfactors = 2, rotate = "oblimin", fm = "pa")
solution 