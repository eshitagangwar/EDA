library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(e1071)
library(readxl)
library(gridExtra)
cd = read_excel("C:/Users/Administrator/Desktop/datasets/Calling Data.xlsx")
l = read_excel("C:/Users/Administrator/Desktop/datasets/Leads.xlsx")
ed = read_excel("C:/Users/Administrator/Desktop/datasets/Enrollment Data.xlsx")
id = read_excel("C:/Users/Administrator/Desktop/datasets/ID Table.xlsx")
str(ed)
str(l)
View(l)
str(cd)
me = merge(cd , l , by.x ='Lead ID' , by.y ='Lead ID' , all.x = TRUE )
str(me)
me%>%
colnames(me)
dim(me)
dim(l)
dim(cd)
mk = cd%>%summarise(t = n_distinct(`Lead ID`))
str(mk)
dim(mk)
View(mk)
k = l%>%summarise(t = n_distinct(`Lead ID`))
View(k)
mle =   merge(l , ed , by.x ='Lead ID' , by.y ='Lead ID' , all.x = TRUE )
m1 = l%>%summarise(t = n_distinct(`Lead ID`))
colnames(mle)
j = mle%>%group_by(`Lead Source ID`)%>%summarise(t1 = n())%>%arrange(-t1)
View(j)
med = ed%>%filter(Total >=100000)
med%>%summarise(t = n_distinct(`Lead ID`))
str(ed)
dim(ed)
str(med)
(dim(med))
ml =   merge(med , cd , by.x ='Lead ID' , by.y ='Lead ID' , all.x = TRUE )
colnames(ml)
str(m1)
dim(ml)
View(ml)
kk = ml%>%group_by(`Lead ID`)%>%summarise(u = n())
View(kk)
dim(kk)
mean(kk$u)
kkl = cd%>%group_by(`Lead ID`)%>%summarise(called = n())
mean(kkl$called)
cd%>%summarise(n = n_distinct(`Lead ID`))
View(kkl)
mer = merge(ml , kkl , by.x = "Lead ID" , by.y = "Lead ID" , all.x = T)
dim(kkl)
View(mer)
dim(mer)
colnames(mer)
mer%>%summarise( k = n_distinct(`Lead ID`))
View(mer[,c("Lead ID","called")])
 l1 = distinct(mer)
dim(l1) 
lll = mer%>%group_by(called)%>%summarise(jj = n())
dim(lll)
View(lll)
colnames(cd)
yu = merge(l , cd , )



# Find the counsellor name who has maximum leads which are not  even once ?


# 






















