# Linear Regression
str(elantratest)

LinReg = lm(ElantraSales~Unemployment+ CPI_all+ CPI_energy+ Queries, data=elantratrain)
summary(LinReg)

# adding month as a numeric variable to observe impact on output
LineReg2 = lm(ElantraSales~Month + Unemployment+ CPI_all+ CPI_energy+ Queries, data=elantratrain)
summary(LinReg)

# Update data set to convert month in a factor variable
elantratest$monthfact = as.factor(elantratest$Month)
elantratrain$monthfact = as.factor(elantratrain$Month)

LinReg3 = lm(ElantraSales~monthfact + Unemployment+ CPI_all+ CPI_energy+ Queries, data=elantratrain)
summary(LinReg3)

#Testing for Correlation
cor(elantratrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
cor(elantratrain)

# Reducing the model
LinRegRed = lm(ElantraSales~monthfact + Unemployment+ CPI_all+ CPI_energy, data=elantratrain)
summary(LinRegRed)

# Prediction
prediction = predict(LinRegRed,newdata = elantratest)

# R2 for Test
SSE = sum((prediction - elantratest$ElantraSales)^2)
SSE

Baseline = mean(elantratrain$ElantraSales)
Baseline

SST = sum((mean(elantratrain$ElantraSales) - elantratest$ElantraSales)^2)
R2 = 1-SSE/SST
R2

# Biggest error and biggest error month
max(abs(prediction - elantratest$ElantraSales))
which.max(abs(prediction - elantratest$ElantraSales))