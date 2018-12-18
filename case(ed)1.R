library(dplyr)
h = read.csv('C:/Users/Administrator/Desktop/HR Analytics.csv')
q1 = h%>% group_by(Gender,Attrition)%>%summarise(t = n())
View(q1)
summary(hr)
ctable = table(h$Gender ,h$Attrition)
ctable1 = rbind(ctable,total =colSums(ctable))
ctable1 = cbind(ctable1,totalrow =rowSums(ctable1))
ctable1
prop.table(prop.table(ctable,2),2)
gtablee = table(h$Attrition,h$Department,h$Age)
gtablee

##############
t = round(prop.table(table(h$Department, h$Attrition)),2)
t = table(h$Department, h$Attrition,h$Gender)

t = as.data.frame.matrix(t)

colnames(t) = c("Currently working", "Left the company")

t
str(hr)
###################################BAR CHART######################
library(ggplot2)
ggplot(h , aes(x = EducationField)) +geom_bar(width = .5 , aes(fill = EducationField)) + theme(axis.text.x = element_text(angle = 45), legend.position = "bottom"  ) + scale_fill_manual(labels = c("HR","LS","MK","MD","OT","TD"))
 ########
data3 = mpg
w1 = data3%>%group_by(manufacturer,class)%>%summarise(count = n())
View(w1)


ggplot(hr, aes(x= JobRole,aes(fill = Attrition) )) + geom_bar(position = "fill", width =.6) + theme(axis.text.x = element_text(angle = 45))























