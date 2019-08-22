############### Line Chart ####################################################################
# Data Set used: ODI
# Source of data set: Data.world
# Problem statement: Ploting Month wise number of matches
# Bivariate Analysis

library(ggplot2)

data1 = economics

# Ploting a line chart

chart1 = ggplot(data1, aes(x=date, y = unemploy)) + geom_line()

chart1 

# Plotting multiple line charts - Option 1

chart2 = ggplot(data1, aes(x = date)) + 
  geom_line(aes(y = unemploy), col = "Red") +
  geom_line(aes(y = pce), col = "Green")

chart2 

# Or Option 2

chart3 = ggplot(data1, aes(x = date)) + 
  geom_line(aes(y = unemploy, color = "Unemployment")) + 
  geom_line(aes(y = pce, color = "Price"))

chart3 

# My chart is showing data over a period of 10 years. I want to show for each year

library(lubridate)

brks <- data1$date[seq(1, length(data1$date), 12)]
lbls <- lubridate::year(brks)

chart4 + scale_x_date(labels = lbls, breaks = brks) + theme(axis.text.x = element_text(angle = 90))
