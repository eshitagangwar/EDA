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

rules = apriori(Groceries , parameter = list(supp = 0.001 , conf = 0.08 ,maxlen =3), appearance = list(default ="lhs",rhs ="whole milk"))
options(digits = 2)

rules = sort(rules ,decreasing = TRUE , by = "confidence")
inspect(rules[1:5])
# Important words parameter

# Support : occurence of A with  B in all trans 
# p() = p(a intersection b)/total trans

# Confidence:

#
#lhs(already bought)
#rhs (will buy)
rules = apriori(Groceries , parameter = list(supp = 0.001 , conf = 0.5 ,maxlen =3), appearance = list(default ="lhs",rhs ="tropical fruit"))
options(digits = 2)

rules = sort(rules ,decreasing = TRUE , by = "confidence")
inspect(rules[1:5])
# lhs                                    rhs              support confidence lift count
# [1] {pip fruit,misc. beverages}         => {tropical fruit} 0.0013  0.62       5.9  13   
# [2] {turkey,root vegetables}            => {tropical fruit} 0.0015  0.60       5.7  15   
# [3] {domestic eggs,semi-finished bread} => {tropical fruit} 0.0010  0.59       5.6  10   
# [4] {UHT-milk,hygiene articles}         => {tropical fruit} 0.0010  0.59       5.6  10   
# [5] {pip fruit,cat food}                => {tropical fruit} 0.0014  0.58       5.6  14 
rules = sort(rules ,decreasing = TRUE , by = "count")
inspect(rules[1:5])
