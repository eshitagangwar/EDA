library(psych)
library(ggplot2)

ty = read.csv('C:/Users/Administrator/Desktop/datasets/tyre.csv')
str(ty)

# to know the data 

#
describeBy(ty$Mileage , group = ty$Brands, digits = 2 , mat = TRUE)

#
describeBy(ty$Mileage , group = ty$Brands, digits = 2 , mat = FALSE)

#
boxplot( ty$Brands , ty$Mileage)
# no outliers in mileage
ggplot(ty , aes(x = Brands , y = Mileage)) +geom_boxplot()
# is there any outlier present inside particular brand
# null hypothesis : Mileage doesn't have to do with Brand
# alt hyothesis :   Mileage depends upon the Brand
model =aov(Mileage ~ Brands , ty)
summary(model)
# accept the alt



##################### TUKEY HST TEST ############################

test = TukeyHSD(model , conf.level = .95 , ordered = TRUE)
test
plot(test , col = "red" ,las = 1 , margin.table(1,1,1,3))

da = chickwts
model1 = aov(weight ~ feed ,da)
#null hypothesis accepted
summary(model1)
test1 = TukeyHSD(model1 , conf.level = .95 ,ordered = TRUE)
test1
plot(test1)
# reject the null hypothesis intially
str(da)
library(dplyr)
da%>%group_by(feed)%>%summarise(y = n())%>%View()
# the data is normally distributed 
# the data is equally spaced
test2 = test1$feed
library(readxl)
sm = read_excel('C:/Users/Administrator/Desktop/datasets/Students Marks.xlsx')
library(tidyr)
marks_long = gather(sm , Assessment_type , Marks_obtain , FA:IA)
View(marks_long)
str(marks_long)
names = c('Student' , 'Assessment_type')
marks_long[,names] = lapply(marks_long[,names], factor)
str(marks_long)
View(marks_long)
######### 
model1 = aov(Marks_obtain ~ Student + Assessment_type ,marks_long )
summary(model1)
test2 = TukeyHSD(model1 ,conf.level = .95)
test2
plot(test2)
View(test2)
# accept

model = aov(marks_long , Marks_obtain ~ Student + Assessment_type)

#################  TIME-SERIES############
economics
e = economics
str(e)
dim(e)
library(ggplot2)
View(e)
k =ggplot(e , aes(x = date )) + geom_line(aes( y = unemploy ,color ='unemployement'))
k
# to draw the average line
o = k + geom_line(aes( y = pce ,color ='price') )
o
# breaks
br = e$date[seq(1,length(e$date),10)]
str(br)
dim(e)
lbs = lubridate::year(br)
str(lbs)
l = o + scale_x_date(labels = lbs , breaks = br) + theme(axis.text.x = element_text(angle = 90))
l
odi = read.csv("C:/Users/Administrator/Desktop/datasets/odi-batting.csv")
str(odi)
odi$month = as.Date(odi$MatchDate , '%m-%d-%Y')
View(odi$month)
odi$month = format(odi$month , '%b')
o = odi%>%group_by(month)%>%summarise(total_runs = sum(Runs , na.rm = T , T))
ggplot(o , aes(x = month , y = total_runs )) + geom_bar(stat = 'Identity') + scale_x_discrete(limits = month.abb)
