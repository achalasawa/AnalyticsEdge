loans = read.csv("loans.csv")
str(loans)
summary(loans)

table(loans$not.fully.paid)

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
str(missing)

library(mice)

set.seed(144)
?setdiff
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
vars.for.imputation
imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

loans2 = read.csv("loans_imputed.csv")
summary(loans)
summary(loans2)


set.seed(144)
library(caTools)
split = sample.split(loans2$not.fully.paid, SplitRatio = 0.70)
Train = subset(loans2, split == TRUE)
Test = subset(loans2, split == FALSE)

mod1 = glm(not.fully.paid ~ ., data = Train, family = binomial)
summary(mod1)

# Calculating predicted risk
Test$predicted.risk = predict(mod1, newdata=Test, type="response")
table(Test$not.fully.paid, Test$predicted.risk > 0.5)

# Calculating Area under curve
library(ROCR)
pred = prediction(Test$predicted.risk, Test$not.fully.paid)
as.numeric(performance(pred,"auc")@y.values)

# Training a bivariate model
bivariate = glm(not.fully.paid~int.rate, data=Train, family="binomial")
summary(bivariate)
pred2 = predict(bivariate, newdata = Test, type = "response")

which.max(pred2)
str(pred2)
pred2[1780]

pred3 = prediction(pred2, Test$not.fully.paid)
as.numeric(performance(pred3,"auc")@y.values)

Test$profit = exp(Test$int.rate*3) - 1
Test$profit[Test$not.fully.paid == 1] = -1

which.max(Test$profit)
Test$profit[1780]*10

highinterest = subset(Test, int.rate >= 0.15)
Test$profit = exp(Test$int.rate*3) - 1
mean(highinterest$profit)
table(highinterest$not.fully.paid)

# Lets make an investment in some loans, which ones should we invest in?
cutoff = sort(highinterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highinterest, predicted.risk <= cutoff)
nrow(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)