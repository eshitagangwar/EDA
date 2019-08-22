# Load the libraries

library(arules)
library(arulesViz)

# Load the data set

data(Groceries)

# inspecting the data

inspect(head(Groceries, 3))

# (OPTIONAL) If you want to convert your transactions class to a data frame

T = as(Groceries, "matrix")

T = as.data.frame(T)

# (OPTIONAL): If you already have your transactions stored as a dataframe, 
# you could convert it to class transactions as follows,

tData = as (myDataFrame, "transactions") # Optional step

# Some usefull step to check the transaction class data
# number of items in each observation

size(head(Groceries)) 

# Three important terms in MBA

# Support = Number of transactions with both A and B / Total number of transactions = P(A???B)
# Confidence = Number of transactions with both A and B / Total number of transactions with A = P(A???B)/P(A)
# ExpectedConfidence = Number of transactions with B / Total number of transactions = P(B)
# Lift = Confidence / Expected Confidence = P(A???B)/P(A).P(B)

# Plotting the frequently purchased items

itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency") 

# Lets create the rules with support = .001 and confidence = .8

# Get the rules
rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits

options(digits=2)

inspect(rules[1:5])

# if someone buys yogurt and cereals, they are 81% likely to buy whole milk too.

summary(rules)

# The number of rules generated: 410
# The distribution of rules by length: Most rules are 4 items long
# The summary of quality measures: interesting to see ranges of support, lift, and confidence.

# Sorting the rules based on confidence

rules = sort(rules, by="confidence", decreasing=TRUE)

inspect(rules[1:5])

# How To Control The Number Of Rules in Output ?

rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen=3))

summary(rules)

# Using LHS and RHS to find the key insights

# What are customers likely to buy before buying whole milk

rules = apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08, maxlen=3), 
               appearance = list(default="lhs",rhs="whole milk"))

rules = sort(rules, decreasing=TRUE,by="confidence")

inspect(rules[1:5])

# What are customers likely to buy if they purchase whole milk?

rules = apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08, maxlen=3), 
                appearance = list(default="rhs",lhs="whole milk"))

rules = sort(rules, decreasing=TRUE,by="confidence")

inspect(rules[1:5])

# Lets Visulaize

plot(rules,method="graph",interactive=TRUE,shading=NA)














