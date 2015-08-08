# loading data set

FluTrain=read.csv("FluTrain.csv")
summary(FluTrain)
str(FluTrain)

FluTest = read.csv("FluTest.csv")
summary(FluTest)
str(FluTest)

# understanding the data set
max(FluTrain$ILI)
which.max(FluTrain$ILI)
FluTrain$Week[303]

# histogram
FluTrainHist= hist(FluTrain$ILI)

# Plot log to reduce for skewness in ILI
plot(FluTrain$Queries, log(FluTrain$ILI))