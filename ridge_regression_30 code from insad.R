train = read.csv("train2016.csv")
test = read.csv("test2016.csv")

train$YOB = ifelse(!is.na(train$YOB) & train$YOB < 1930, NA, train$YOB)
train$YOB = ifelse(!is.na(train$YOB) & train$YOB > 2000, NA, train$YOB)

test$YOB = ifelse(!is.na(test$YOB) & test$YOB < 1930, NA, test$YOB)
test$YOB = ifelse(!is.na(test$YOB) & test$YOB > 2000, NA, test$YOB)

train[is.na(train$YOB), "YOB"] = round(mean(train$YOB, na.rm = TRUE))
test[is.na(train$YOB), "YOB"] = round(mean(test$YOB, na.rm = TRUE))

library(caTools)
set.seed(123)
spl = sample.split(train$Party, SplitRatio = 0.8)
dataTrain = subset(train, spl == TRUE)
dataTest = subset(train, spl == FALSE)

formula = Party ~ Income + HouseholdStatus + EducationLevel + Gender + Q109244 + Q115611 + Q113181 + Q115195 + Q98197 - 1

library(glmnet)
x.train = model.matrix(formula, data = dataTrain)
y.train = dataTrain$Party
x.test = model.matrix(formula, data = dataTest)

#ridge = glmnet(x.train, y.train, family = "binomial", alpha = 0)
#plot(ridge, xvar = "lambda", label = TRUE)

cv.ridge = cv.glmnet(x.train, y.train, family = "binomial", alpha = 0)
plot(cv.ridge)

predTest = predict(cv.ridge, newx = x.test, s = "lambda.min")
predTestLabels = as.factor(ifelse(predTest < 0, "Democrat", "Republican"))

table(dataTest$Party, predTestLabels)
(403 + 323) / nrow(dataTest)

test$Party = ""
x.submit = model.matrix(formula, data = test)

predSubmit = predict(cv.ridge, newx = x.submit, s = "lambda.min")
predSubmitLabels = as.factor(ifelse(predSubmit < 0, "Democrat", "Republican"))
submission = data.frame(USER_ID = test$USER_ID, Predictions = predSubmitLabels)
write.csv(submission, "ridge_regression_30.csv", row.names = FALSE)
