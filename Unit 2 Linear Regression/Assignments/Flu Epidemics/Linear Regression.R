# Linear Regression Model

FluTrend1 = lm (log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

# Log(ILI) in the FluTrend1 object
PredTest1 = predict(FluTrend1 ,newdata = FluTest)
summary(PredTest1)
str(PredTest1)

# Taking the exponent to account for Log(ILI)
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

#Estimate of percentage of ILI related physician visits for week of March 11 2012
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

# Performance on Test Set
(FluTest$ILI[11] - PredTest1[11])/(FluTest$ILI[11])

# RMSE
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
sqrt(mean((PredTest1-FluTest$ILI)^2))