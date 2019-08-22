library(party)

### partation of data into train and test

set.seed(1234)

### dividing the data into train and test

pd = sample(2,nrow(d), replace = TRUE, prob = c(.8,.2))

### Train data set

train = d[pd==1,]

### Test data set

test = d[pd==2,]

### Creating tree using ctree

tree = ctree(train$Species~ ., data = train)

plot(tree)

## predicting the model

P = predict(tree, test[,1:4])

### Confusion matrix

tab = table(P, test$Species)
tab











