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

# Installing ROCR Package - Receiver Operating Characteristic
install.packages("ROCR")
library(ROCR)

# Creating a new prediction object with ROCR
ROCRpred= prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf= performance(ROCRpred, "tpr", "fpr")

# Plotting ROCR performance curve
plot(ROCRperf)

# Adding colour and legend to the line graph
plot(ROCRperf, colorize = TRUE)

# Adding threshold values to the line graph
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

#Calculating the Area under the ROC curve
predictTest = predict(qualitylog, type="response", newdata=qualityTest)
summary(predictTest)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

auc
