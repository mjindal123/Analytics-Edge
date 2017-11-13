Fluetrain = read.csv("FluTrain.csv")
FluTest= read.csv("FluTest.csv")
which.max(Fluetrain$ILI)
Fluetrain$Week[303]

which.max(Fluetrain$Queries) 
hist(Fluetrain$ILI)
plot(Fluetrain$Queries, log(Fluetrain$ILI))
FluTrend1 = lm(log(ILI)~Queries, data=Fluetrain)
summary(FluTrend1)

PredTest1 = predict(FluTrend1, newdata=FluTest)
# However, the dependent variable in our model is log(ILI), 
# so PredTest1 would contain predictions of the log(ILI) value. 
# We are instead interested in obtaining predictions of the ILI value. 
# We can convert from predictions of log(ILI) to predictions of ILI via exponentiation, 
# or the exp() function. The new code, which predicts the ILI value, is

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))

#Next, we need to determine which element in the test set is for March 11, 2012. We can determine this with:
  which(FluTest$Week == "2012-03-11 - 2012-03-17")
#Now we know we are looking for prediction number 11. This can be accessed with:
  PredTest1[11]
  SSE = sum((PredTest1-FluTest$ILI)^2)

  RMSE = sqrt(SSE / nrow(FluTest))
  install.packages("zoo")
  library(zoo)
  ILILag2 = lag(zoo(Fluetrain$ILI), -2, na.pad=TRUE)
  ts.plot(Fluetrain$ILILag2)
  Fluetrain$ILILag2 = coredata(ILILag2)
  plot(log(Fluetrain$ILILag2), log(Fluetrain$ILI))
  
  FluTest$ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
  FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=Fluetrain)
  summary(FluTrend2)
  
  PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
  SSE = sum((PredTest2-FluTest$ILI)^2)
  RMSE = sqrt(SSE / nrow(FluTest))
  