#Question 2
#1)	Apply the k-means algorithm to get 4 clusters.
#Remember to set the seed to 1234 so the results are comparable.
#Note that you can remove few column, which are not used in K-means.
titanic = read.csv('C:/Users/Administrator/Desktop/datasets/Titanic.csv')
set.seed(1234)
library(broom)
library(class)
library(caTools)
titanic_new=titanic%>%select(-Name,-Sex,-Embarked,-Cabin,-Ticket,-SibSp)
View(titanic_new)
titanic_new$PassengerId=as.numeric(titanic_new$PassengerId)
titanic_new$Survived=as.numeric(titanic_new$Survived)
titanic_new$Pclass=as.numeric(titanic_new$Pclass)
titanic_new$Parch=as.numeric(titanic_new$Parch)
View(titanic_new)
str(titanic_new)
scale(titanic_new)
set.seed(1234)
titanic_final=as.data.frame(titanic_new)
cluster4= kmeans(titanic_final,4)
table(titanic$Sex,titanic$Survived,cluster4$cluster)

#2)	Describe, in words, each of the clusters.
#cluster1, 84.4% male is died and 30% female died
#cluster2, 74% male is died and 25% female survied
#cluster3, 78% male is died and 21% female survied
#cluster4, 80.6% above male is died and 22.5% female died


#3)	Now, 4 clusters was an arbitrary choice. What seems like a reasonable number of clusters?
cluster1 = kmeans(titanic_final,1)
cluster2 = kmeans(titanic_final,2)
cluster3 = kmeans(titanic_final,3)
cluster4 = kmeans(titanic_final,4)
cluster5 = kmeans(titanic_final,5)
cluster6 = kmeans(titanic_final,6)
cluster7 = kmeans(titanic_final,7)
cluster8 = kmeans(titanic_final,8)
cluster9 = kmeans(titanic_final,9)
cluster10 = kmeans(titanic_final,10)
cluster11 = kmeans(titanic_final,11)
cluster12 = kmeans(titanic_final,12)
cluster13 = kmeans(titanic_final,13)
cluster14 = kmeans(titanic_final,14)
cluster15 = kmeans(titanic_final,15)

y = c(cluster1$tot.withinss,cluster2$tot.withinss,cluster3$tot.withinss,cluster4$tot.withinss,cluster5$tot.withinss,cluster6$tot.withinss,cluster7$tot.withinss,cluster8$tot.withinss,cluster9$tot.withinss,cluster10$tot.withinss,cluster11$tot.withinss,cluster12$tot.withinss,cluster13$tot.withinss,cluster14$tot.withinss,cluster15$tot.withinss)
x = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
plot(x, y, xlab = "No. of Cluster", ylab = "Total withiness", type = "b", col="red")