library(ggplot2)
library(dplyr)
d = mpg

b = boxplot(d$hwy,d$cty , names = c('hwy','cty'))
b
str(d)

# removing the outliers
d1 = d%>%filter(hwy<40)%>%filter(cty<28)
View(d1)
boxplot(d1$cty,d1$hwy)
kk =d1%>%filter(hwy>40)%>%filter(cty>28)
boxplot(d1$hwy)
# capping the data with high_wisker
ihwy =IQR(d$hwy)
q1 = quantile(d$hwy , 0.25)
q1
q2 = quantile(d$hwy , 0.75)
lw = q1 - 1.5*ihwy
lw
hw = q2 + 1.5*ihwy
hw
d$out = ifelse(d$hwy > hw | d$hwy <lw , 'Yes' , 'No')
View(d)
k = d%>%mutate(new_hwy = ifelse(out == 'Yes' , hw ,hwy))
boxplot(k$new_hwy)
str(k)

# replacing with mean and median
o = d%>%mutate(new_hwy = replace(hwy , hwy>40 , mean(40)))
boxplot(o$new_hwy)

