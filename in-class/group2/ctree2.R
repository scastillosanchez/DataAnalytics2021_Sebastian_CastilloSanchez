#Sebastian Castillo-Sanchez
#ctree2

#plotting pair plot for swiss data with an education subset
pairs(~ Fertility + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")
require(party)
#plotting ctree or kind of decision tree
swiss_ctree <- ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_ctree)


