# Answering questions

plot(statedata$x,statedata$y)

tapply(statedata$HS.Grad,statedata$state.region,mean)

boxplot(statedata$Murder ~ statedata$state.region)

NortheastData = subset(statedata, state.region == "Northeast")
NortheastData

Reg1 = lm(Life.Exp ~ Population+ Income+ Illiteracy+ Murder+ HS.Grad+ Frost+ Area, data = statedata)
summary(Reg1)

plot(statedata$Income, statedata$Life.Exp)

Reg2 = lm(Life.Exp ~ Population+ Illiteracy+ Murder+ HS.Grad+ Frost+ Area, data = statedata)
summary(Reg2)
Reg3 = lm(Life.Exp ~ Population+ Murder+ HS.Grad+ Frost, data = statedata)
summary(Reg3)

sort(predict(Reg3))

which.min(statedata$Life.Exp)
statedata$state.name[40]

statedata$state.name[which.max(statedata$Life.Exp)]

sort(abs(Reg3$residuals))