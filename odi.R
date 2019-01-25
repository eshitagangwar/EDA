library(ggplot2)
library(dplyr)
data1= mtcars
str(data1)
View(data1)
odi = read.csv('C:/Users/Administrator/Desktop/datasets/odi-batting.csv')

#normal histogram


h= hist(odi$Runs , xlab = "Runs bin", ylab = "Freq",labels = TRUE)
h$perc = (h$counts/(sum(h$counts)))*100
plot(h,freq = FALSE,labels = TRUE)


# mean when data is normal distribution otherwise it will be  misleading


mean(odi$Runs,na.rm = T)
median(odi$Runs,na.rm = T)
library(moments)
library(psych)
skew(odi$Runs)
skewness(odi$Runs,na.rm = T)
skewness(odi$ScoreRate,na.rm = T)



# Convert to positive skewed data to normal data

odi$Runs_sqrt = sqrt(odi$Runs)
View(odi)
hist(odi$Runs_sqrt)
skewness(odi$Runs_sqrt,na.rm = T)
mean(odi$Runs_sqrt,na.rm = T)
median(odi$Runs_sqrt,na.rm = T)
boxplot(odi$Runs_sqrt1)


##### still slightly skewd , again do the sqrt


odi$Runs_sqrt1 = sqrt(odi$Runs_sqrt)
hist(odi$Runs_sqrt1)
skewness(odi$Runs_sqrt1,na.rm = T)
mean(odi$Runs_sqrt1,na.rm = T)
median(odi$Runs_sqrt1,na.rm = T)




####### it is becoming more skewed 



data2 = mpg
View(data2)
str(data2)
k = boxplot(data2$cty)
k = data2$cty
describe(k)
k


############# boxplot using ggplot


ggplot(data2 , aes(class, cty)) + geom_boxplot(outlier.colour = "Red")

########## adding a 

ggplot(data2 , aes(class, cty)) + geom_boxplot(aes(fill = as.factor(cyl)),outlier.colour = "Red" )



as = data2%>%filter(class =='compact' & cyl == '4' )%>%select(cty)
View(as)
summary(as)
w = boxplot(as)
min(as)
max(as)
quantile(as$cty ,c(0.25,0.5,0.975))
IQR(as$cty)
as = data2%>%filter(class =='suv' & cyl == '6' )%>%select(cty)
View(as)
summary(as)
min(as)
quantile(as$cty ,0.75)
k = IQR(as$cty)+ quantile(as$cty ,0.75)
k
l=0
str(data2)
colnames(data2)
for( i in colnames(data2)){
  for(k in seq(1,6)){
    print(i[k])
    l=l+1
  }
  
  
  
  
#########################################################
library()
}
print(l)
head(data2)
