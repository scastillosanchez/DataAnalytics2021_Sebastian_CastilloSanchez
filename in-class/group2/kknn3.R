#Sebastian Castillo-Sanchez
#kknn3

#load in data
data(swiss)
#produce pairplot
pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")

