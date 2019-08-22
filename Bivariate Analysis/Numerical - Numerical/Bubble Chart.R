############ Bubble chart #################################################################
# Data Set used: mpg
# Source of Data : R - Inbuild data set
# Problem Statement: Find the corelation between displacement and mileage (Both city and highway mileage) of various class.
# Bivarate Analysis

chart = ggplot(mpg, aes(x = displ, y = cty)) + geom_jitter(aes(col = class, size = hwy)) + 
  labs(subtitle="Displacement v/s Mileage", y="City Mileage", x="Displacement", 
       title="Bubble Chart")

chart
