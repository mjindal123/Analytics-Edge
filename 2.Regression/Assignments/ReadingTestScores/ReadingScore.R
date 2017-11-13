pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
tapply(pisaTrain$readingScore,pisaTrain$male,mean)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
# Unordered factor - we have to create binary variable(IV) for each variable except reference variable.

str(pisaTrain)

# Because the race variable takes on text values, it was loaded
# as a factor variable when we read in the dataset with read.csv() -- 
#   you can see this when you run str(pisaTrain) or str(pisaTest). However, 
# by default R selects the first level alphabetically ("American Indian/Alaska Native") 
# as the reference level of our factor instead of the most common level ("White"). 
# Set the reference level of the factor by typing the following two lines in your R console:
#   
  pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmscore = lm(readingScore ~ ., data = pisaTrain)
summary(lmscore)

SSE = sum(lmscore$residuals^2)
#and then dividing by the number of observations and taking the square root:
  RMSE = sqrt(SSE / nrow(pisaTrain))
RMSE
predTest = predict(lmscore, newdata=pisaTest)
sum((predTest-pisaTest$readingScore)^2)
sqrt(mean((predTest-pisaTest$readingScore)^2))


baseline = mean(pisaTrain$readingScore)
sum((baseline-pisaTest$readingScore)^2)
