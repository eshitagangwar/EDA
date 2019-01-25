library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(e1071)
#library(miscset)
#1.	What is the average age of the employees in each department excluding the outliers?
hr = read.csv('C:/Users/Administrator/Desktop/datasets/HR Analytics.csv')
str(hr)
c1 = hr%>%group_by(Department)%>%summarise(ma = mean(Age), me = median(Age))
View(c1)
boxplot(hr$Department,hr$Age)
ggplot(hr , aes(x = hr$Department , y = hr$Age)) +geom_boxplot()
#2.	Which education field has the highest attrition rate for the employees with 
#less than 5 years of work experience and monthly salary between 2000 and 4000 USD.
c2 = hr%>%filter(MonthlyIncome >2000 , MonthlyIncome <4000 , TotalWorkingYears <5)%>%
  group_by(EducationField)%>%summarise(t = sum(Attrition , na.rm = T)/n()*100)
View(c2)
#3.	What is the average salary hike for the employees with work experience between 8 years to 10 years.
c3 = hr%>%filter( TotalWorkingYears <=10 ,  TotalWorkingYears >=8)%>%
  summarise(t = mean((PercentSalaryHike)*(MonthlyIncome)/100 , na.rm = T))
View(c3)

ggplot(hr , aes(x = MonthlyIncome , y = TotalWorkingYears)) + geom_jitter(aes(color = as.factor(Attrition)))
ggplot(hr , aes(x = PercentSalaryHike , y = MonthlyIncome)) + geom_jitter(aes(color = as.factor(Attrition)))
#4.	Is the "number of companies worked" normally distributed for the employees with work
# experience between 8 years and 10 years.
c4 = hr%>%filter( TotalWorkingYears <=10 ,  TotalWorkingYears >=8)
View(c4)
skewness(c4$NumCompaniesWorked) 
hist(c4$NumCompaniesWorked)
#no  skewness present positive skewness
#5.	Divided the whole data into two groups - 
#Group1: people with monthly salary more than the average salary of all employees and people with 
#monthly salary less than the average salary of all employees. For each group analysis the attrition rate
#in each Job Role. 
str(hr)
k = mean(hr$MonthlyIncome)
k
p = hr%>%filter(hr$MonthlyIncome > k)
p1 = hr%>%filter(hr$MonthlyIncome < k)
str(p)
View(p1)
ab=ggplot(p ,aes(x = JobRole  )) + geom_bar(aes(fill = as.factor(Attrition)) , position ='fill') +theme(axis.text.x = element_text(angle = 45))
bl=ggplot(p1 ,aes(x = JobRole  )) + geom_bar(aes(fill = as.factor(Attrition)) , position ='fill') +theme(axis.text.x = element_text(angle = 45))
library(gridExtra)
grid.arrange(ab,bl)
bl
str(hr)
hr%>%group_by(JobRole)%>%summarise(t = mean(MonthlyIncome))%>%View()
