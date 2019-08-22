# Problem Statement: Test the hypothesis whether the students smoking habit is independent of their exercise level 
# Data Set: Survey
# Data Source: Inbuil in MASS lib

# HO : Smoking habit is independent of exercise level
# H1 : Smoking habit is depend on exercise level

library(MASS)

data = survey

View(data)

tbl = table(survey$Smoke, survey$Exer) 

tbl

chisq.test(tbl) 

# The warning message found in the solution above is due to the small cell values in the contingency table. 
# To avoid such warning, we combine the second and third columns of tbl, and save it in a new table named ctbl.

ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) 

ctbl 

# Rename the column

colnames(ctbl) = c("Freq", "None/Some")

# Re-applying chi-square test

chisq.test(ctbl)

