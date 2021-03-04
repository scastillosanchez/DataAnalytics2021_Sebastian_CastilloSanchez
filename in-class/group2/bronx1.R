#Sebastian Castillo-Sanchez
#bronx 1

#load necessary libraries
library(gdata) 
library(readxl)

#read in data and attach dataset
bronx1 <- read_xls("data/rollingsales_bronx.xls", skip = 4, .name_repair = "universal")
str(bronx1)
attach(bronx1)

#make variables numeric
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 

#change 0 values to null due to log error in regression
GROSS.SQUARE.FEET[GROSS.SQUARE.FEET == 0] <- NA
SALE.PRICE[SALE.PRICE == 0] <- NA

#plot variables and regression of gross sq ft on sale price
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

#change 0 values to null due to log error in regression
bronx1$GROSS.SQUARE.FEET[bronx1$GROSS.SQUARE.FEET == 0] <- NA
bronx1$SALE.PRICE[bronx1$SALE.PRICE == 0] <- NA
bronx1$LAND.SQUARE.FEET[bronx1$LAND.SQUARE.FEET == 0] <- NA

#plot regression
m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))

# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
