library(arules)
 data("Groceries")
# trasaction data , not the data frame
class(Groceries)

# 
View(inspect(head(Groceries,5)))

T = as(Groceries,"matrix")

T = as.data.frame(T)
View(T)
#
tdata =  as()

(s = size(head(Groceries,3)))
class(s)

itemFrequencyPlot(Groceries , topN =10 , type ="absolute" , main ="Item Frequency")

rules = apriori(Groceries , parameter = list(supp = 0.01 , conf = 0.8))

# Important words parameter

# Support : occurence of A with  B in all trans 
# p() = p(a intersection b)/total trans

# Confidence:

#
 