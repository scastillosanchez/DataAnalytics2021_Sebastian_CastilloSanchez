#Lab 1
#Sebastian Castillo-Sanchez

#Using GPW3 GRUMP data

GPW3_data <- read.csv("data/GPW3_GRUMP_SummaryInformation_2010.csv") #read in GPW3 data
GPW3_data <- GPW3_data[1:232,] #cut unnecessary/blank rows on bottom
head(GPW3_data)
summary(GPW3_data)
str(GPW3_data)

fivenum(GPW3_data$Resolution, na.rm=TRUE)
stem(GPW3_data$Resolution)


#Plotting

#Histograms
jpeg(file="normal_hist_GPW3.jpeg")
hist(GPW3_data$Resolution)
dev.off()

jpeg(file="second_hist_GPW3.jpeg")
hist(GPW3_data$Resolution, seq(0., 400., 20.0), prob = TRUE)
rug(GPW3_data$Resolution)
lines(density(GPW3_data$Resolution, na.rm = TRUE, bw=1.))
dev.off()

#Density
plot(ecdf(GPW3_data$Resolution), do.points = FALSE, verticals = TRUE)

plot(dt(GPW3_data$Resolution, df = 1)) #t distribution

#Boxplot and Q-Q plot

boxplot(GPW3_data$Resolution)
qqplot(GPW3_data$Resolution, GPW3_data$NumUnits)

#Filter by UN Regions
GPW3_NA <- GPW3_data$Resolution[GPW3_data$UNRegion == "Northern America"]