# loading data set
parole = read.csv("parole.csv")

table(parole$violator)

str(parole)

# converting state and crime to factors
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

summary(parole)

# Creating test and control data set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# first logistic regression model
mod1 = glm(violator ~ ., data = train, family = binomial)
summary(mod1)

# first predictions on test data set based on mod1
pred1 = predict(mod1, newdata = test, type = "response")
max(pred1)

# confusion matrix
table(test$violator, as.numeric(pred1 > 0.5))

table(test$violator)

# Calculating area under curve
library(ROCR)
ROCRPred = prediction(pred1, test$violator)

as.numeric(performance(ROCRPred, "auc")@y.values)