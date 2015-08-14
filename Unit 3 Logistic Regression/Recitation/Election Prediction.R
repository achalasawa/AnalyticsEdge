# uploading data
polling = read.csv("PollingData.csv")
str(polling)
summary(polling)

# how much data we have ?
table(polling$Year)

# handling missing data - imputation
install.packages("mice")
library(mice)

simple = polling [c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)

set.seed(144)

# Imputing is so simple
imputed = complete(mice(simple))

summary(imputed)

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

summary(polling)

Train= subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

table(Train$Republican)

# Lets create a smarter baseline model
sign(20)
sign(-10)
sign(0)

table(sign(Train$Rasmussen))

table(Train$Republican, sign(Train$Rasmussen))

# Checking for multi collinearity
cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

# 1 variable model
mod1 = glm(Republican ~ PropR, data= Train, family = "binomial")
summary(mod1)
# making predictions on training to compare with baseline
pred1 = predict(mod1, type = "response")
# baseline comparison
table (Train$Republican, pred1 >= 0.5)
  
# 2 variable model
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data= Train, family = "binomial")
summary(mod2)
# making predictions on training to compare with baseline
pred2 = predict(mod2, type = "response")
# baseline comparison
table (Train$Republican, pred2 >= 0.5)

# predicting on test data set

table(Test$Republican, sign(Test$Rasmussen))

TestPrediction = predict(mod2, newdata = Test, type = "response")
summary(TestPrediction)
table(Test$Republican, TestPrediction >= 0.9)

subset(Test, TestPrediction >= 0.5 & Republican ==0)
