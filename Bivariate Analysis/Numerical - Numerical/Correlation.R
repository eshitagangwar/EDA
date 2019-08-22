########### Correlogram ###########################################################################
# Data Set used: mtcars
# Category: Relationship (Bivariate Analysis/Multivariate)
# No. of dimension presented: 0
# No. of measure (Matric) presented: 2 or more
# Special remark: Mainly used to find the correlation between two or more numerical varibale.
# Problem statement : To find the correlation between various variable 
# and dividing them into to hierarchical clustering.


library(corrplot)
data_cor = mtcars

# First think to draw a correlogram is to create a correlation matrix between various variables.

str(data_cor)

Matrix = cor(data_cor)

# Ploting the correlation plot

chart9 = corrplot(Matrix, method = "square", type = "upper")

# Reordering the correlation matrix
# The correlation matrix can be reordered according to the correlation coefficient. 
# This is important to identify the hidden structure and pattern in the matrix. 
# "hclust" for hierarchical clustering order is used for this purpose.

chart9.1 = corrplot(Matrix, method = "color", type = "upper" , order = "hclust")

chart9.1