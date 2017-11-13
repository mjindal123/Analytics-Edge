loans = read.csv("loans.csv")
prop.table(table(loans$not.fully.paid))
summary(loans)

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)
#The model can be trained and summarized with the following commands:
  mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)
test$predicted.risk = predict(mod, newdata=test, type="response")
table(test$not.fully.paid, test$predicted.risk > 0.5)


library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)


pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)



prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)
test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
