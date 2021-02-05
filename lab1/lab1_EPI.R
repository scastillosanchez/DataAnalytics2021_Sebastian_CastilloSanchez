#Lab 1
#Sebastian Castillo-Sanchez

#Using EPI 2010 data

library(readxl) #library to read in excel files

EPI_data <- read_excel("data/EPI_data_2010.xls", sheet = "EPI2010_onlyEPIcountries") #read in EPI data
head(EPI_data)
str(EPI_data)

#Exercise 1 

#THIS IS FOR EPI COLUMN
#Similar take to the one from the lecture slides
#----------------------------------------
summary(EPI_data$EPI)
fivenum(EPI_data$EPI, na.rm=TRUE)
stem(EPI_data$EPI)

jpeg(file="normal_hist_EPI.jpeg")
hist(EPI_data$EPI)
dev.off()

jpeg(file="second_hist_EPI.jpeg")
hist(EPI_data$EPI, seq(30., 95., 1.0), prob = TRUE)
rug(EPI_data$EPI)
lines(density(EPI_data$EPI, na.rm = TRUE, bw=1.))
dev.off()

par(pty = "s")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)

plot(ecdf(EPI_data$EPI), do.points = FALSE, verticals = TRUE)


#----------------------
#BELOW IS FOR BIODIVERSITY COLUMN
#--------------------------
summary(EPI_data$BIODIVERSITY)
fivenum(EPI_data$BIODIVERSITY, na.rm=TRUE)
stem(EPI_data$BIODIVERSITY)

jpeg(file="normal_hist_BIODIVERSITY.jpeg")
hist(EPI_data$BIODIVERSITY)
dev.off()

jpeg(file="second_hist_BIODIVERSITY.jpeg")
hist(EPI_data$BIODIVERSITY, seq(0., 100., 1.0), prob = TRUE)
rug(EPI_data$BIODIVERSITY)
lines(density(EPI_data$BIODIVERSITY, na.rm = TRUE, bw=1.))
dev.off()

plot(ecdf(EPI_data$BIODIVERSITY), do.points = FALSE, verticals = TRUE)

#-------------------------------------
#BELOW IS FOR THE FISHERIES COLUMN
#----------------------------------
EPI_data$FISHERIES <- as.numeric(as.character(EPI_data$FISHERIES))
summary(EPI_data$FISHERIES)
fivenum(EPI_data$FISHERIES, na.rm=TRUE)
stem(EPI_data$FISHERIES)

jpeg(file="normal_hist_FISHERIES.jpeg")
hist(EPI_data$FISHERIES)
dev.off()

jpeg(file="second_hist_FISHERIES.jpeg")
hist(EPI_data$FISHERIES, seq(20., 100., 1.0), prob = TRUE)
rug(EPI_data$FISHERIES)
lines(density(EPI_data$FISHERIES, na.rm = TRUE, bw=1.))
dev.off()

plot(ecdf(EPI_data$FISHERIES), do.points = FALSE, verticals = TRUE)

#--------------------------------------------------
#Inter-comparing some columns distributions
boxplot(EPI_data$ENVHEALTH, EPI_data$BIODIVERSITY, EPI_data$ECOSYSTEM)
boxplot(EPI_data$EPI, EPI_data$DALY)
boxplot(EPI_data$WATER_H, EPI_data$WATER_E, EPI_data$AIR_H, EPI_data$AIR_E)

qqplot(EPI_data$BIODIVERSITY, EPI_data$ECOSYSTEM)
qqplot(EPI_data$WATER_H, EPI_data$WATER_E)
qqplot(EPI_data$AIR_H, EPI_data$AIR_E)
#--------------------------------------------------------------

#EXERCISE 2
#Filtering by specific columns

EPILand <- EPI_data$EPI[!EPI_data$Landlock] #Not Landlocked
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30.,95.,1.0), prob = TRUE)

EPIWater <- EPI_data$EPI[!EPI_data$No_surface_water] #no surface water
EWater <- EPIWater[!is.na(EPIWater)]
hist(EWater)
hist(EWater, seq(30.,95.,1.0), prob = TRUE)

EPIDesert <- EPI_data$EPI[!EPI_data$Desert] #Does it have a desert?
EDesert <- EPIDesert[!is.na(EPIDesert)]
hist(EDesert)
hist(EDesert, seq(30.,95.,1.0), prob = TRUE)

EPIPop <- EPI_data$EPI[!EPI_data$High_Population_Density] #High pop density?
EPop <- EPILand[!is.na(EPIPop)]
hist(EPop)
hist(EPop, seq(30.,95.,1.0), prob = TRUE)


#Filter by EPI_regions
EPI_Latin <- EPI_data$EPI[EPI_data$EPI_regions == "Latin America and Caribbean"]

#Filter by GEO_subregion
EPI_GEO_WE <- EPI_data$EPI[EPI_data$GEO_subregion == "Western Europe"]

