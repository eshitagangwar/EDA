################################ K-means ##############################################

# Using Inbuild data set Iris

data1 = iris

# Removing the last column

library(dplyr)

data2 = select(data1, -c(Species))

# In this case all the columns have same scale. In case if the columns are not on same scale,
# we have to standardize our data. This can be done as below: 

# data3 = scale(data2)
# stand_data = as.data.frame(data3)

# Creating a K-means cluster. 
# In this case I know we have 3 different species.

results = kmeans(data2, 3)

# Lets interpret the results

results

# Adding the cluster column to our data frame

data2$Cluster = results$cluster

# Ploting the cluster
# We will plot the graph between petal.length and petal.width

plot(data2$Petal.Length, data2$Petal.Width, col = data2$Cluster)


### In case if you do not know number of cluster. How to find the same.

# We use Elbow method to find the number of clusters required. There are two way to plot elbow graph.

# Way 1: Easy and long

elbow_data = select(data1, -c(Species))

# Create the k-means clustering using 2, 3, 4, 5, 6, 7, 8, 9, 10 clusters
results1 = kmeans(elbow_data, 1)
results2 = kmeans(elbow_data, 2)
results3 = kmeans(elbow_data, 3)
results4 = kmeans(elbow_data, 4)
results5 = kmeans(elbow_data, 5)
results6 = kmeans(elbow_data, 6)
results7 = kmeans(elbow_data, 7)
results8 = kmeans(elbow_data, 8)
results9 = kmeans(elbow_data, 9)
results10 = kmeans(elbow_data, 10)

# Creating two array to plot the curve between Number of clusters and results$tot.withinss

x = array(c(1,2,3,4,5,6,7,8,9,10))

y = array(c(results1$tot.withinss, results2$tot.withinss, results3$tot.withinss, results4$tot.withinss, results5$tot.withinss, 
            results6$tot.withinss, results7$tot.withinss, results8$tot.withinss, 
            results9$tot.withinss, results10$tot.withinss))

plot(x,y, type = "b", xlab="Number of Clusters", ylab="Distortion", col="Red")

# Way 2: Complex and short by creating a UDF

kmeans.wss.k <- function(elbow_data, k) {
  km = kmeans(elbow_data,k)
  return (km$tot.withinss)
}

#Plotting the elbow graph with different values of km$tot.withinss

kmeans.dis = function(elbow_data, maxk){
  dis=(nrow(elbow_data)-1)*sum(apply(elbow_data,2,var))
  dis[2:maxk]=sapply (2:maxk, kmeans.wss.k, elbow_data=elbow_data)
  return(dis)
}
maxk = 10
dis = kmeans.dis(elbow_data, maxk);
plot(1:maxk, dis, type='b', xlab="Number of Clusters",
     ylab="Distortion",
     col="Red")




