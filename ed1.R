str(mtcars)
ctable = table(mtcars$gear ,mtcars$am)
#table(row, col)
ctable
#######Adding the new col and row

ctable = rbind(ctable,total =colSums(ctable))
ctable = cbind(ctable,totalrow =rowSums(ctable))
ctable
########### what percent are automatic?
prop.table(ctable,2)
View(ctable)
 ############
###### 1 - row
####### 2 - col
### - grand total
ctable1 = table(mtcars$gear ,mtcars$cyl)
ctable1
prop.table(prop.table(ctable1,2),2)
ctable1 = rbind(ctable1,total =colSums(ctable1))
ctable1 = cbind(ctable1,totalrow =rowSums(ctable1))

gtablee = table(mtcars$gear, mtcars$cyl, mtcars$am ,mtcars$vs)
######## long data
View(gtablee)
prop.table(gtablee,1)

###### LONG DATA INTO WIDE DATA
library(readxl)
wide = read_excel('C:/Users/Administrator/Desktop/datasets/wide1.xlsx')
View(wide)
library(tidyr)
l=gather(wide )
View(l)
l1 = gather(wide, Week, Weight, week1:week3)
View(l1)
#######
s = spread(l1,Week,Weight)
View(s)

#
gtablee = as.data.frame(gtablee)
gtable_wide = spread(gtablee ,Var3 ,Freq)
View(gtable_wide)
data11 = mtcars
library(rpivotTable)
#
rpivotTable(data11)









