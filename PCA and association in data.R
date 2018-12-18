a=c(3,-4,-6,0,1,0,1,2,-2)
b=matrix(a,nrow=3)
a
b

c=c(2,2,-1)
d=c(-1,0,2)
e=c(1,0,1)
f=c(3,2,1)

b%*%c
b%*%d
b%*%e
b%*%f

e1=eigen(b)
e1
e1$values
e1$vectors

library(readxl)

d=read_excel("D:/dataset/pcd1.xlsx")
View(d)

d$hours_mod=((d$hours-mean(d$hours)))/(sd(d$hours))
d$marks_mod=((d$marks-mean(d$marks)))/(sd(d$marks))
d1=d[3:4]
var1=var(d1)
var1
a=eigen(var1)
a

#direct
d2=prcomp(d,scale=T)
d2$rotation

d2$sdev
var(d)
loadings(d2)
plot(d2,type='lines')

#########Association in data#####

install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

da1=data("Groceries")

###inspecting the data
inspect(head(Groceries, 3))

####if u want to convert the transaction class to a dataframe

t=as(Groceries,"matrix")
t=as.data.frame(t)
View(t)

####if u want to convert datafram to transaction data

tdata=as(t,"transactions")
tdata

###to check a transaction data
###number of items in each observation

size(head(Groceries, 3))
##three important terms in MBA
#######support=number of transaction with both A and B/ total number of transaction=p(A???B)
###confidence=no of transaction with both A and B/total no of transaction with A=p(A???B)
###expectedconfidence=no of transaction with B/total no of transaction=p(B)
####lift=confidence/expected confidence=p(A???B)/p(B)

####plotting the frequently purchased item

itemFrequencyPlot(Groceries,topN=10,type="absolute",main="Item Frequency")

#lets create the rules with support=.001 and confidence=.8

#get the rules

rules=apriori(Groceries,parameter = list(supp=0.01,conf=.8))
##this is a AND operator...with support and confidence
#####output-------------writing ... [0 rule(s)] done [0.00s].


rules=apriori(Groceries,parameter = list(supp=0.001,conf=.8))##this is a AND operator...with support and confidence
  ###output------writing ... [410 rule(s)] done [0.00s].


rules=sort(rules,decreasing = TRUE,by="confidence")
inspect(rules[1:5])

rules

summary(rules)

##lets visualize
plot(rules,method = "graph",interactive = TRUE,shading = NA)

