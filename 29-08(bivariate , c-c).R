library(MASS)
data = survey




# H0 : smoking doesnt  depend on the exercise 
# H1 : smoking depend on the exercise
ctab = table(data$Smoke,data$Exer )
ctab
chisq.test(ctab)
# greater then 0.05 hence accept


# sample size should be comparable
ctab1 = cbind(ctab[,"Freq"] , ctab[,"None"]+ctab[,"Some"])
ctab1
chisq.test(ctab1)
# accept the null hypothesis


data1 = mtcars
data1$carb = NULL
data1$gear = NA
str(data1)
str(mtcars)
