library(arules)
library(arulesViz)
data("Groceries")

inspect(head(Groceries,3))
t=as(Groceries,'matrix')
t=as.data.frame(t)
View(t)



#plotting the frequently purchased items
itemFrequencyPlot(Groceries,topN=10,type='absolute',main='item frequency')

#get the rules
rules=apriori(Groceries,parameter=list(supp=.01,conf=0.5))

#show the top 5 rules, but only 2 digit
options(digits=2)

inspect(rules[0])
