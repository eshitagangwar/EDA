############# Lollipop Chart ########################################################

# Data Set used: ODI
# Category: Comparison (Univariate Analysis)
# No. of dimension presented: 1
# No. of measure (Matric) presented: 1
# Special remark: Use when no. of dimension to present is more. However, labeling is difficult with lollipop chart

library(readxl)
library(ggplot2)
library(dplyr)

odi = read_excel("E://Training data//Amit//Data Set//odi-batting-analysis.xlsx") 

# Objective is to identified which month had highest count of matches played and ploting them on lollipop chart

# Extracting Month from MatchDate column
str(odi) # Match data column is already in date format.

# Creating a month column and I need month in abbreviation
odi$Month = format(odi$MatchDate, "%b")

# Using dplyr summarise the month wise matches

matches_month_wise = odi %>% group_by(Month) %>% summarise(Count = n()) 

# Ploting lollipop chart

chart = ggplot(matches_month_wise, aes(x= Month, y=Count))+ geom_point(size = 5) +  
  geom_segment(aes(x=Month, xend=Month, y=0, yend=Count)) + 
  labs(title="Lollipop Chart", subtitle="Month Wise Matches", 
       caption="source: Data.World") + scale_x_discrete(limits = month.abb) +
  theme_bw()
chart
