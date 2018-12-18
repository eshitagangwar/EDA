# K-means clustering using iris data set
# Not for prediction only for classifcation
#
data = iris
str(data)
d_new = data[,1:4]
str(d_new)

# Lets divide the data into3
# unit of the col are different, biased towards with more unit or higher unit
#when data is not on same scale, transfer into one , like 1 in hours and 2 in sec then change both in h or in sec
# scaling or normalisation
# z-score

# Lets standardized
d_new = scale(d_new)
View(d_new)
# u can't do rescaling
# compare to the sepal.lenght , petal.width make sense now

set.seed(1234)
# for same type of o/p 
# randomness in each the group will be same



result = kmeans(d_new , 3)
result

# centroid values might be row 
result$centers

#
result$tot.withinss

set.seed(1234)
result1 = kmeans(data[,1:4],3)
result1
result1$centers

mean(data[1:50,]$Sepal.Length)

data$cluster = result1$cluster
View(data)
#simple join table
table(data$Species,data$cluster)

plot(data$Sepal.Length,data$Sepal.Width ,col = result1$cluster )
l = c()
y = c(1,2,3,4,5,6,7,8,9,10)
for (i in y){
  
  r = kmeans(data[,1:4],i)
  print(r$tot.withinss)
  l[i]  =r$tot.withinss
}
View(l)
plot(y,l)
# 4 is optimal

library(party)
set.seed(1234)
pd  = sample(2,nrow(data),replace = TRUE, prob = c(.8,.2))#  sample wll give the index in two sample we want
View(pd)
train= data[pd==1,]
str(test)
test = data[pd==2,]
tree = ctree(train$Species~.,data = train)
plot(tree)
# z test value is p value
data = iris
p = predict(tree ,test[,1:4])
View(p)
tab = table(test$Species,p)
tab
# validation dataframe
c = colnames(data)
c
val.data = data.frame(Sepal.Length = 8 , Sepal.Width =4.5 , Petal.Length = 2.9 ,Petal.Width = 1.2)
val.data
p1 =predict(tree,val.data)
p1
