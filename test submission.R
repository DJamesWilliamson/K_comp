# Test submission using imputed data (imp3_set1), logistic regression, all variables
# Import data
train <- read.csv("train_imp3_set1.csv")
test <- read.csv("test_imp3_set1.csv")
str(train)
str(test)

# Change name of train Party variable; and include USER_ID
train2016 <- read.csv("train2016.csv", na.strings = c("", "NA"))
test2016 <- read.csv("test2016.csv", na.strings = c("", "NA"))
str(train2016)
str(test2016)

train$X <- train2016$USER_ID
names(train)[1] <- "USER_ID"
names(train)[ncol(train)] <- "Party"

test$X <- test2016$USER_ID
names(test)[1] <- "USER_ID"

# Order factors
train$YOB <- ordered(train$YOB,
                     levels = c("(1928,1957]", "(1957,1967]", "(1967,1973]", 
                                "(1973,1978]", "(1978,1983]", "(1983,1987]",
                                "(1987,1991]", "(1991,1994]", "(1994,1996]", "(1996,2001]"))
train$Income <- ordered(train$Income,
                          levels = c("under $25,000", "$25,001 - $50,000",
                                     "$50,000 - $74,999", "$75,000 - $100,000",
                                     "$100,001 - $150,000", "over $150,000"))
train$EducationLevel <- ordered(train$EducationLevel,
                                  levels = c("Current K-12", "High School Diploma",
                                             "Current Undergraduate", "Associate's Degree",
                                             "Bachelor's Degree", "Master's Degree",
                                             "Doctoral Degree"))
test$YOB <- ordered(test$YOB,
                     levels = c("(1928,1957]", "(1957,1967]", "(1967,1973]", 
                                "(1973,1978]", "(1978,1983]", "(1983,1987]",
                                "(1987,1991]", "(1991,1994]", "(1994,1996]", "(1996,2001]"))
test$Income <- ordered(test$Income,
                        levels = c("under $25,000", "$25,001 - $50,000",
                                   "$50,000 - $74,999", "$75,000 - $100,000",
                                   "$100,001 - $150,000", "over $150,000"))
test$EducationLevel <- ordered(test$EducationLevel,
                                levels = c("Current K-12", "High School Diploma",
                                           "Current Undergraduate", "Associate's Degree",
                                           "Bachelor's Degree", "Master's Degree",
                                           "Doctoral Degree"))

# Basic model to predict Party using all other variables in the dataset, 
# except for the first, X:
SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)
summary(SimpleMod)

# Make predictions on the test set:
PredTest = predict(SimpleMod, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)

TestSubmission = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
write.csv(TestSubmission, "TestSubmission.csv", row.names=FALSE)
