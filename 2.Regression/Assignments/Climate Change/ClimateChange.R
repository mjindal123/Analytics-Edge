climate = read.csv("climate_change.csv")
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(climatelm)
LinReg = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(LinReg)
#We have many variables in this problem, and as we have seen above, 
# dropping some from the model does not decrease model quality. R provides a 
# function, step, that will automate the procedure of trying different combinations 
# of variables to find a good compromise of model simplicity and R2. This trade-off is 
# formalized by the Akaike information criterion (AIC) - it can be informally thought of 
# as the quality of the model with a penalty for the number of variables in the model.


StepModel = step(climatelm)


# It is interesting to note that the step function does not address the 
# collinearity of the variables, except that adding highly correlated variables 
# will not improve the R2 significantly. The consequence of this is that the step 
# function will not necessarily produce a very interpretable model - just a model that 
# has balanced quality and simplicity for a particular weighting of quality and simplicity (AIC).

tempPredict = predict(StepModel, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum((mean(train$Temp)-test$Temp)^2)
1-SSE/SST
