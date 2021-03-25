#pca lab
#Sebastian Castillo-Sanchez

#read in iris data

data('iris')
head(iris)

irisdata1 <- iris[,1:4]
irisdata1

#finding the principal components
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)

#plotting the principal components in different ways

plot(principal_components)
plot(principal_components, type = "l") #elbow plot
biplot(principal_components) #biplot


# loading in boston dataset
data(Boston, package="MASS")

#performing pca
pca_out <- prcomp(Boston,scale. = T)
pca_out

#plotting the pca
plot(pca_out)
biplot(pca_out, scale = 0)

#viewing the pca values 
boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)


#read in usarrests data
data("USArrests")
states=row.names(USArrests) 
states

#apply mean and variance to data
names(USArrests )
apply(USArrests , 2, mean)
apply(USArrests , 2, var)

#perform pca
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

#view center,scale and rotation components of prcomp
pr.out$center
pr.out$scale
pr.out$rotation


#view dimensions of the pca values
dim(pr.out$x)

#plot two components with a scale of 0
biplot(pr.out, scale=0)

#view standard deviation of pca and variance
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var

#find proportion of variance in pca
pve = pr.var/sum(pr.var)
pve



