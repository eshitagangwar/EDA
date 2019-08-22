############ Area chart ############################################
# Data Set used: Economics data set
# Source of data set: R-Inbuild data set
# Problem statement: To draw stacked area chart for Unemployment and Price
# Bivariate Analysis

data1 = economics

library(ggplot2)

chart = ggplot(data1, aes(x=date)) + 
  geom_area(aes(y=unemploy, fill="Unemployment"))
  
chart

############ Stacked Area chart ############################################
# Data Set used: Economics data set
# Source of data set: R-Inbuild data set
# Problem statement: To draw stacked area chart for Unemployment and Price
# Bivariate Analysis

chart1 = ggplot(data1, aes(x=date)) + 
  geom_area(aes(y=unemploy, fill="Unemployment")) + 
  geom_area(aes(y=pce, fill="Price"))

chart1
