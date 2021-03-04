#Sebastian Castillo-Sanchez
#kmeans1

#load in data
data(swiss)

#conduct kmeans of data 
sclass <- kmeans(swiss[2:6], 3) 
table(sclass$cluster, swiss[,1])   #view clusters 
