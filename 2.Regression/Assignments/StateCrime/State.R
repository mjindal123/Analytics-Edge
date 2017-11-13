statedata = read.csv("statedata.csv")

plot(statedata$x, statedata$y) 
tapply(statedata$HS.Grad,statedata$state.region,max)
boxplot(statedata$Murder ~ statedata$state.region) 
boxplot(statedata$Murder ~ statedata$state.region) 
NortheastData = subset(statedata, state.region == "Northeast")
plot(NortheastData$Murder,NortheastData$state.abb)

LinReg = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
