# Time Series forecasting
install.packages("zoo")
library(zoo)

# Creating a lag function
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)

plot(FluTrain$ILILag2, log(FluTrain$ILI))

# Training a new regression model
FluTrend2 = lm(log(ILI) ~ Queries+log(ILILag2),data=FluTrain)
summary(FluTrend2)

# Updating the test data set with ILILag2
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(ILILag2)

# Adding missing values
head(FluTest, n =10)
tail(FluTrain, n =10)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

# Predicting the time series value of all values from 2012 on test data set
prediction = exp(predict(FluTrend2,newdata=FluTest))

SSE = sum((prediction - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
sqrt(mean((prediction-FluTest$ILI)^2))