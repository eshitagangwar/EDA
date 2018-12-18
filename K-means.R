library(party)

d=iris
View(d)

d_new=d[,1:4]
View(d_new)

#lets devide the data into 3 clusters
#lets standard the data
#d_new=scale(d_new)
#View(d_new)

#creating the clusters
set.seed(1234)
result=kmeans(d_new,3)#3 is no of clusters
result

d2=d_new
View(d2)
set.seed(1234)
result=kmeans(d2,3)#3 is no of clusters
result
result$centers#centeroids
d2$clust=result$cluster
View(d2)

result$tot.withinss


#finding the optimal number of cluster

k1=kmeans(d2,1)
k2=kmeans(d2,2)
k3=kmeans(d2,3)
k4=kmeans(d2,4)
k5=kmeans(d2,5)
k6=kmeans(d2,6)
k7=kmeans(d2,7)

k8=kmeans(d2,8)
k9=kmeans(d2,9)
k10=kmeans(d2,10)

error.vec=c(k1$tot.withinss,
            k2$tot.withinss,
            k3$tot.withinss,
            k4$tot.withinss,
            k5$tot.withinss,
            k6$tot.withinss,
            k7$tot.withinss,
            k8$tot.withinss,
            k9$tot.withinss,
            k10$tot.withinss)

cluster.vec=c(1,2,3,4,5,6,7,8,9,10)
library(party)
plot(cluster.vec,error.vec,type ='b')

#partition data into train and test

set.seed(1234)

#deviding the data into 2 sample so that it can test and train
pd=sample(2,nrow(d),replace=TRUE,prob=c(.8,.2))
pd
#train data set
train=d[pd==1,]
View(train)
#test data set
test=d[pd==2,]
View(test)
#creating tree using ctree
tree=ctree(Species~.,data=train)
plot(tree)

#Predictiong the model
p=predict(tree,test[,1:4])
View(p)
#confusion matrix
tab=table(p,test$Species)
tab

#lets try to classify the new data
val.data=data.frame(Sepal.Length=8,Sepal.Width=4.5,Petal.Length=2.5,Petal.Width=1.2)
View(val.data)

p2=predict(tree,val.data)
p2=predict(val.data,test[,1:4])
p2



data=read.csv('C:/Users/Administrator/Desktop/datasets/binary.csv')
str(data)
data$admit=as.factor(data$admit)
str(data)

#split it into test and train data
pd=sample(2,nrow(data),replace = TRUE,prob = c(.8,.2))
View(pd)

#train data sets
train=data[pd==1,]
View(train)

#test data sets
test=data[pd==2,]
View(test)

#logistic regression neeeds a categorial output variable
model=glm(admit~.,data=train,family=binomial('logit'))
model

#pridecting on test data
test$pred = predict(model,test[,2:4],type='response')

test$pred=round(test$pred,2)
test$pred_admit=ifelse(test$pred>.5,'admitted','not admitted')

#prob prediction for each row of test data set
##transit,spasssmatrix
table(test$admit, test$pred_admit)

#lets predict with validation data
val.data=data.frame(gre=180, gpa=2.1, rank=3)
predict(model,val.data,typr='response')#only 4% chance of geting admission
