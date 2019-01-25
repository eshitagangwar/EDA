library(ggplot2)
library(dplyr)
library(gridExtra)
hr = read.csv('C:/Users/Administrator/Desktop/datasets/HR Analytics.csv')
# To find relationship between city milage and highway mileage
s =ggplot(mpg , aes(x = cty , y = hwy)) + geom_point(aes(color = class))

j =ggplot(mpg , aes(x = cty , y = hwy)) + geom_jitter(aes(color = class))
j = ggplot(mpg , aes(x = cty , y = hwy)) + geom_jitter(aes(color = class, size = cyl))
j
# reversing the numerical and categorical in aes
j = ggplot(mpg , aes(x = cty , y = hwy)) + geom_jitter(aes(color = cyl, size = class))
j
# higher the cty , higher the hwy 
# which class have wat kind of relation ship
grid.arrange(s,j)
# how to calculate the linear relationship
# fourth parameter
str(mpg)

# co-relation matrixs
datasets::mtcars
View(mtcars)
M = cor(mtcars)
View(M)
# No of col is greater
library(corrplot)
corrplot(M , method = 'square' , type = 'upper', order = 'hclust')
######## highly co related in the form of cluster
library(purrr)
View(str(hr))
name = c("Education", "Attrition","JobInvolvement" ,"TrainingTimesLastYear","EmployeeNumber","JobSatisfaction","JobRole","JobLevel","PerformanceRating","WorkLifeBalance","RelationshipSatisfaction","EnvironmentSatisfaction")
hr[,name] = lapply(hr[,name], factor)
k = hr%>%keep(is.numeric)
str(hr)
dim(k)
View(k)
m1 = cor(k)
corrplot(m1 , type = 'upper', order = 'hclust' , tl.cex = 0.75)
######## model #############
Model = lm(hr$MonthlyIncome~hr$TotalWorkingYears )
summary(Model)
Model1 = lm(hr$MonthlyIncome~hr$NumCompaniesWorked)
summary(Model1)
Model2 = lm(hr$MonthlyIncome~hr$TotalWorkingYears+hr$NumCompaniesWorked)
summary(Model2)
#############
# education field vs mean of the MonthlyIncome
c = hr%>%group_by(EducationField)%>%summarise(t = mean(MonthlyIncome))
View(c)
w = ggplot(c , aes(x = reorder(EducationField, t) , y = t)) + geom_bar(stat = "Identity" , width = .5 , aes(fill = EducationField)) + coord_flip()
w
e = ggplot(c , aes(x = reorder(EducationField, -t) , y = t)) + geom_point(size = 5)
e
i = ggplot(c , aes(x = reorder(EducationField, -t) , y = t)) + geom_point(size = 5) + geom_segment(aes(x = EducationField ,xend = EducationField ,y=0 ,yend = t)) + geom_text(aes(label = t ) , position = position_stack(vjust =1.1))
i
#ggplot(c , aes(x = reorder(EducationField, -t) , y = t)) + geom_point(size = 5) + geom_bar(stat ="Identity" , width = 0.1)                                                                                                  

##############
da = chickwts
View(da)
library(stats)
# Null Hypothesis : Meal type effect the chick weight
# All. Hypothesis : Meal tpye doesn't have any effect on the chick weight
model = aov(weight ~ feed , da) 
summary(model)
# reject Null Hypothesis
library(moments)
skewness(da$weight)
barplot(da$weight)
boxplot(da$weight)
# model practice
model
str(k)
skewness(hr$Age,na.rm = T)

m1 = aov( Age ~ Attrition , hr )
t1 =summary(m1)
t = unlist(t1)
View(t) 
t
t2 = as.data.frame(t["Pr(>F)1"])
colnames(t2) = c("Model 1")
t2
str(hr)
str(k)
# one category through all;
# uni- variate, skewness, outlier , bi-variate(N,N) , annova, normalisation
