library(psych)
library(ggplot2)
# lets import the data set

tyre = read.csv("E://Training data//PGDS- EDA//tyre.csv")

str(tyre)

# The dependent variable Mileage is numeric and the independent variable Brand is of type factor.

# Lets check the stats of each group

describeBy(tyre$Mileage,group = tyre$Brand, digits = 2, mat = TRUE)

# Lets also check the distribution of each tyre type using box plot

ggplot(tyre, aes(x = Brands, y = Mileage)) + geom_boxplot(outlier.colour = "Red")

# we can see from describe function and box plot that it certainly appears that we have evidence of the effect of 
# tire brand on mileage. There is one outlier for the CEAT brand but little cause for concern. 
# Means and medians are close together so no major concerns about skewness.
# Point to check is: Is the difference in the mean mileage is Statistics significant. Thus we will perform One - Way ANOVA

# H-Null : There is no statistically significant difference in the mileage of different tyre types. 
# Which means that type of types does not effect mileage
# H-Alternate: There is statistically significant difference in the mileage of different tyre types. 
# Which means that the type of tyres does effect the mileage.

# Lets perfom the ONE WAY ANOVA

model = aov(Mileage~Brands, data = tyre)

# Lets check the summary of the model

summary(model)

# Interpretation of One way ANOVA
# Use the significance level to decide whether to reject or fail to reject the null hypothesis (H0). 
# When the p-value is less than the significance level (in this case we will take it as 0.05, indicates a 5% 
# risk of concluding that a difference exists when there is no actual difference.), 
# the usual interpretation is that the results are statistically significant, and you reject H0.

# As the P-values is less than the significance level, we can reject the null hypothsis. 
# Which means, the type of tyres does effect the mileage.

# Now we would like to understand, the difference in average mileage in which group is statistically significant.
# we will perform the Turkey's Honest Significant Difference (HSD) Test.

test = TukeyHSD(model, conf.level = .95, ordered = FALSE)

# Output of TukeyHSD says,
# 1. Difference between CEAT tyre and Bridgestone tyre mean mileage is 2.98, with CEAT mean mileage 2.98 more than Bridgestone.
# 2. The 95% confidence interval of that difference is between 0.87 and 5.09 points.
# 3. p -adjusted value is .002 which is less than significance value of .005 means NULL hypothesis can be reject for this combination.
# 4. For the combination of Apollo - CEAT, p -adjusted value is greater than .005 mean NULL hypothesis is accepted for that combination.

# We can also plot the output of TukeyHSD test

plot(test, col = "red", las = 1)
















