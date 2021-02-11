#Lab 1 Part 2
#Sebastian Castillo-Sanchez

library(ggplot2)
data("diamonds") #using diamonds data set from ggplot2 package

head(diamonds)

hist(diamonds$carat)
hist(diamonds$price)

ggplot(diamonds, aes(x=price)) + geom_histogram(binwidth = 500)

ggplot(diamonds, aes(x=factor(cut))) + geom_bar()

ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
ggplot(diamonds, aes(x=carat, y=price, color = clarity)) + geom_point()

ggplot(diamonds, aes(x=x, y=y)) + geom_point() + geom_line()

ggplot(diamonds, aes(x=color, y=price)) + geom_boxplot()
ggplot(diamonds, aes(x=cut, y=price)) + geom_boxplot()

qplot(diamonds$color)





