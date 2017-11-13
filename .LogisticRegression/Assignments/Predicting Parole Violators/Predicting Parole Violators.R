parole = read.csv("parole.csv")
table(parole$violator)
str(parole)
summary(parole)
head(parole)

# Using the as.factor() function, convert these variables to
# factors. Keep in mind that we are not changing the values, just the 
# way R understands them (the values are still numbers).

parole$state = as.factor(parole$state)#
parole$crime = as.factor(parole$crime)
str(parole)


 set.seed(144)

library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
mod = glm(violator~., data=train, family="binomial")
summary(mod)

# The following two properties might be useful to you when answering this question:
#   
#   1) If we have a coefficient c for a variable, then that means the log odds (or Logit) 
# are increased by c for a unit increase in the variable.
# 
# 2) If we have a coefficient c for a variable, then that means the odds are multiplied 
# by e^c for a unit increase in the variable.


# From the logistic regression equation, we have log(odds) = -4.2411574 + 0.3869904*male + 
#   0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 
#   0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*
#   crime2 - 0.2781054*crime3 - 0.0117627*crime4. This parolee has male=1, race=1, age=50, 
# state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, 
# crime3=0, crime4=0. We conclude that log(odds) = -1.700629.
# 
# 
# Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted
# probability of violation is 1/(1+exp(1.700629)) = 0.154.


predictions = predict(mod, newdata=test, type="response")
summary(predictions)

table(test$violator,predictions>0.5)

12/23 #sen
167/179 #spec
179/202 #accuracy

library(ROCR)
pred = prediction(predictions, test$violator)
as.numeric(performance(pred, "auc")@y.values)
