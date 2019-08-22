# Lets import the data 

library(readxl)
library(tidyr)

marks = read_excel("E://Training data//PGDS- EDA//Students Marks.xlsx")

# Converting the same to long data

marks_long = gather(marks, Assessment_type, Marks_obtain, FA:IA)

# Lets check the data type

str(marks_long)

# We need to convert the column of students and Assessment_type to factor

marks_long$Student = as.factor(marks_long$Student)
marks_long$Assessment_type = as.factor(marks_long$Assessment_type)

str(marks_long)

# Lets check the stats of each group

describeBy(marks_long$Marks_obtain ,group = marks_long$Student, digits = 2, mat = TRUE)

# Lets also check the distribution of each tyre type using box plot

ggplot(marks_long, aes(x = Student, y = Marks_obtain)) + geom_boxplot(aes(fill = Assessment_type), outlier.colour = "Red")

# Describe and boxplot function shows such values as only its without replication.

# Null Hypothesis for Two way anova

# H01: There is no statistically significant difference in the means (Marks obtain) of factor A (Assessment_type)
# H02: There is no statistically significant difference in the means (Marks obtain) of factor B (Students)
# H03: There is no statistically signigicant interaction between factors A and B i.e (This will be only in case of Two way with replication)
# The null hypothesis is that there is no interaction 
# between columns (data sets) and rows. 
# More precisely, the null hypothesis states that any systematic differences between columns are the same for each row 
# and that any systematic differences between rows are the same for each column.

# H11: There is statistically significant difference in the means (Marks obtain) of factor A (Assessment_type)
# H12: There is statistically significant difference in the means (Marks obtain) of factor B (Students)
# H13: There is statistically signigicant interaction between factors A and B ( This will be only in case of two way replication)

# Lets perfom the Two way ANOVA without replication

model = aov(data = marks_long, Marks_obtain~Student+Assessment_type)

# Lets check the summary of the model

summary(model)

# For Two way ANOVA with replication

model = aov(data = marks_long, Marks_obtain~Student+Assessment_type+Student:Assessment_type)



