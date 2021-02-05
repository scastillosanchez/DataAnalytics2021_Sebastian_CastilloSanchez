#Lab 1
#Sebastian Castillo-Sanchez

#Water treatment data

wdata <- read.csv("data/water-treatment.csv", na.strings = "?")
head(wdata)

wdata$DATE = substr(wdata$DATE, 3, 9)
wdata$DATE = as.Date(wdata$DATE, "%d/%m/%y")

str(wdata)
summary(wdata$ZN.E)

hist(wdata$ZN.E)


wdata_above_2000 = wdata[wdata$DATE > "2000-01-01",]
