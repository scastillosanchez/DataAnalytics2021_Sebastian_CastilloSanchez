#ctree lab all parts
#Sebastian Castillo-Sanchez

#part 1
require(rpart)
require(rpart.plot)
#load in swiss data
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
#plotting rpart plot and using specific package
plot(swiss_rpart) 
text(swiss_rpart) 
rpart.plot(Swiss_rpart)

require(party)
#creating ctree and plotting it
treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)
#using cforest function
cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

#tree and cforest for fertility column
treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))

#using tree library
library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)


#part 2
#ctree for mileage cu.summary
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
#plotting ctree with text
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)


#part 3
#ctree for kyphosis data and plotting tree
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")


