#pca wine data

#read in wine data
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)

#check number of rows in data
nrow(wine_data)

#add column names since they were not in data 
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data)

#view heatmap documentation and show heatmap of variable correlations
help("heatmap") 
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 

help(factor) #view factor documentation

#make the cvs column into a factor
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes

Help(prcomp) #view prcomp documentation
help(scale) #view scale documentation

#perform pca on wine data by scaling it first and then excluding the cvs column
wine_data_PCA <- prcomp(scale(wine_data[,-1]))

#view summary of the pca results 
summary(wine_data_PCA)







