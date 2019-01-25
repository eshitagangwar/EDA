NFL = read.csv('C:/Users/Administrator/Desktop/datasets/NFL.csv')

set.seed(1234)
library(broom)
library(class)
library(caTools)
#1)Get the number of missing data points per column

missing_values_count=colSums(is.na(NFL))
View(missing_values_count)

#2)Percent of data that is missing in terms of cells.
percentoverall=sum(is.na(NFL))/prod(dim(NFL))*100
View(percentoverall)
percent=apply(NFL, 2, function(col)sum(is.na(col))/length(col))*100
View(percent)
#3)Find the number of rows after removing all the rows that contain a missing value.
removingrows=na.omit(NFL) %>% glimpse
dim(removingrows)

# 4)remove all columns with at least one missing value
columns_with_na_dropped =NFL[,colSums(is.na(NFL)) == 0] 
View(columns_with_na_dropped)
dim(columns_with_na_dropped)
dim(NFL)
# just how much data did we lose?
sprintf("Columns in original dataset: %s", ncol(NFL))
sprintf("Columns with na's dropped: %s", ncol(columns_with_na_dropped))


#5)Replace all missing value by the mean value of that column.
library("imputeTS")
meanval=na.mean(NFL)
View(meanval)
summary(meanval)
