# Importing the data set

data = read.csv("E://Training data//PGDS- EDA//Data Set//binary.csv")

# Spliting it into test and train data

pd = sample(2,nrow(data), replace = TRUE, prob = c(.8,.2))

### Train data set

train = data[pd==1,]

### Test data set

test = data[pd==2,]

#Logistic regression needs a categorical output variable

train$admit = as.factor(train$admit)

model = glm(admit ~., data = train, family = binomial("logit"))

# Pridecting on test data

test$admit = as.factor(test$admit)

test$Pred = predict(model, test[,2:4], type="response")

test$Pred = round(test$Pred, 2)

# Prob. prediction for each row of test data set

table(test$Pred, test$admit)

# Lets predict with the validation data set

Val.data = data.frame(gre = 180, gpa = 2.1, rank = 3)

predict(model, Val.data, type="response")



