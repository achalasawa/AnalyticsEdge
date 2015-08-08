# Loading the dataset
quality = read.csv("quality.csv")
str(quality)

# How many patients received poor care?
table(quality$PoorCare)

# Install packages to split data set in test and train
install.packages("caTools")
library(caTools)

# Setting random number seed to maintain consistency with lectures
set.seed(88)

# Splitting the data set
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

# Subsetting the data set in test and train
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

nrow(qualityTrain)
nrow(qualityTest)

# Creating a generalized logistic model
qualitylog = glm(PoorCare ~ OfficeVisits + Narcotics, data= qualityTrain, family = binomial)
summary(qualitylog)

# Predicting the outcomes of the train data set
predictTrain = predict(qualitylog, type = "response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

# Answering quick question
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)
