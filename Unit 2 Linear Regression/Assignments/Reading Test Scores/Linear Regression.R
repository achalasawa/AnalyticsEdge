# Tapply function reading average scores of males and females
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

# removing missing data
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# relevel factor variable
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Linear Regression Model
lmScore = lm(readingScore ~ grade + male + raceeth + preschool + expectBachelors + motherHS + motherBachelors + motherWork + fatherHS + fatherBachelors + fatherWork + selfBornUS + motherBornUS + fatherBornUS + englishAtHome + computerForSchoolwork + read30MinsADay + minutesPerWeekEnglish + studentsInEnglish + schoolHasLibrary + publicSchool + urban + schoolSize, data = pisaTrain)
summary(lmScore)

# RMSE - directly
sqrt(mean(lmScore$residuals^2))

# Predicting on test set
predTest= predict(lmScore,newdata=pisaTest)
summary(predTest)

#SSE and RMSE
SSE = sum((predTest-pisaTest$readingScore)^2, na.rm = TRUE)
SSE
RMSE = sqrt(mean((predTest-pisaTest$readingScore)^2, na.rm = TRUE))
RMSE

# Baseline Model & SST
baseline = mean(pisaTrain$readingScore,na.rm= TRUE)
baseline

SST = sum((baseline-pisaTest$readingScore)^2)
SST

# Test Set R Squared for lmScore
R2=1-SSE/SST