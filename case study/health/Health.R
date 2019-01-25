test = read.csv('C:/Users/Administrator/Desktop/datasets/test.csv')
train = read.csv('C:/Users/Administrator/Desktop/datasets/train.csv')
set.seed(1234)
library(broom)
library(class)
library(caTools)
#1) What is the probability for someone age 35 with bmi of 32, to be diabetic?
model=glm(type~age+bmi,data=train,family=binomial("logit"))
model
test$pred1=round(predict(model,test[,c(5,7)],type="response"),2)
View(test)
test$prediction1=ifelse(test$pred1>=0.55,"1","0")
View(test)
test$prediction1=as.factor(as.character(test$prediction1))
View(test)


table(test$type,test$prediction1)
a1=sum(diag(table(test$type,test$prediction)))/nrow(test)
bmi=32
age=35
valid=cbind(bmi,age)
valid=data.frame(valid)
analysing1=predict(model,valid,type="response")
analysing1
# 2) According to your model what is the probability that a woman in your sample is diabetic given age 55, a bmi 37, bp 68 and npreg of 2?
model2=glm(type~age+bmi+bp+npreg,data=train,family=binomial("logit"))
model2
test$pred2=round(predict(model2,test[,c(1,3,5,7)],type="response"),2)
View(test)
test$prediction2=ifelse(test$pred2>=0.50,"1","0")
test$prediction2=as.factor(as.character(test$prediction2))
View(test)


table(test$type,test$prediction2)
a2=sum(diag(table(test$type,test$prediction2)))/nrow(test)

## now calculating the bmi n the age and finding the diabetic or not 
bmi=37
age=55
bp=68
npreg=2

validating=cbind(bmi,age,bp,npreg)
validating=data.frame(validating)
analysing2=predict(model2,validating,type="response")
analysing2
# Q3)For a woman aged 35 and mother of 2 children, by how much does the probability of diabetes increase,
#if her bmi was 35 instead of 25 according to the model?
model3=glm(type~age+bmi+npreg,data=train,family=binomial("logit"))
model3
test$pred3=round(predict(model3,test[,c(1,5,7)],type="response"),2)
View(test)
test$prediction3=ifelse(test$pred3>=0.50,"1","0")
test$prediction3=as.factor(as.character(test$prediction3))
View(test)


table(test$type,test$prediction3)
a3=sum(diag(table(test$type,test$prediction3)))/nrow(test)
## now calculating the bmi, age,nperg and finding the diabetic or not 
bmi=35
age=35
npreg=2

validating=cbind(bmi,age,npreg)
validating=data.frame(validating)
analysing1=predict(model3,validating,type="response")
analysing1
## now calculating the bmi, age,nperg and finding the diabetic or not 
bmi=25
age=35
npreg=2

validating=cbind(bmi,age,npreg)
validating=data.frame(validating)
analysing2=predict(model3,validating,type="response")
analysing2
#difference will give the increase in probability
diff=analysing1-analysing2
diff
#Q4) 4)	What is the accuracy of Models?
a=(a1+a2+a3)/3
a