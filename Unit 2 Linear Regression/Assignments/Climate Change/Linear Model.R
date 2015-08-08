# Linear Model

# Testing for Collinearity
cor(climatechange_train)

# Multi Variate Linear models
Model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climatechange_train)
summary(Model1)


Model2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = climatechange_train)
summary(Model2)

# AIC in a stepwise algorith

AICstep = step(Model1)
summary(AICstep)

# Predicting temperature with Test data set
prediction = predict(AICstep, newdata = climatechange_test)
str(prediction)
SSE = sum((prediction - climatechange_test$Temp)^2)
SST = sum( (mean(climatechange_train$Temp) - climatechange_test$Temp)^2)
R2 = 1 - SSE/SST
R2