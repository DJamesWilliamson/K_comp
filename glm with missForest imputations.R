# glm models using imputed data from missForest, and different variable sets
# Import data
questions <- read.csv("questions.csv", stringsAsFactors = FALSE)
train <- read.csv("mF_train.csv")
test <- read.csv("mF_test.csv")
str(questions)
str(train)
str(test)


# Add USER_ID to both and Party to the train set
train2016 <- read.csv("train2016.csv", na.strings = c("", "NA"))
test2016 <- read.csv("test2016.csv", na.strings = c("", "NA"))
train$USER_ID <- train2016$USER_ID
train$Party <- train2016$Party
test$USER_ID <- test2016$USER_ID
str(train); summary(train)
str(test); summary(test)
names(train)
names(test)

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

# glm10 = glm4: using missForest imputed data

# Variables associated with the outcome (Party) using ChiSq
sigQs <- read.csv("sigQs.csv", stringsAsFactors = FALSE)
sigQs <- sigQs$x
nonq_covs <- c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")
covs <- c(nonq_covs, sigQs)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm10 <- glm(as.formula(model_formula), data = train, family = "binomial")
summary(model_glm10)
model_glm <- model_glm10

# Make predictions on the test set:
PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm10 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
write.csv(Sub_glm10, "Sub_glm10.csv", row.names=FALSE)
################################################################################
################################################################################

# glm11 = glm4: using missForest imputed data where both train/test imp together
# Add party variable back into the training set
train <- train_mF_imp2
train$Party <- train2016$Party      
test <- test_mF_imp2
# Variables associated with the outcome (Party) using ChiSq
sigQs <- read.csv("sigQs.csv", stringsAsFactors = FALSE)
sigQs <- sigQs$x
nonq_covs <- c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")
covs <- c(nonq_covs, sigQs)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm11 <- glm(as.formula(model_formula), data = train, family = "binomial")
summary(model_glm11)
model_glm <- model_glm11

# Make predictions on the test set:
PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm11 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
write.csv(Sub_glm11, "Sub_glm11.csv", row.names=FALSE)
################################################################################

# glm12 (=glm8 with mF2 imputation where train/test were imputed together)

# Set up covariates to include in the analysis: use frequent covariates
cov_frequent <- read.table("cov_frequent.txt")
cov_frequent <- as.character(cov_frequent$V1)
covs <- cov_frequent

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm12 <- glm(as.formula(model_formula), data = train, family = "binomial")
summary(model_glm12)
model_glm <- model_glm12

PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm12 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
write.csv(Sub_glm12, "Sub_glm12.csv", row.names=FALSE)


# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm12_back = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
write.csv(Sub_glm12_back, "Sub_glm12_back.csv", row.names=FALSE)

# # Refine model with step function (forwards)
# nothing <- glm(Party ~ 1, data = test, family = "binomial")
# 
# forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm12)), 
#                  direction = "forward")
# 
# PredTest = predict(forwards, newdata=test, type="response")
# threshold = 0.5
# PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))
# 
# #Prepare submission (note will need to incorporate USER_ID from original test2016)
# Sub_glm12_forw = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
# write.csv(Sub_glm12_forw, "Sub_glm12_forw.csv", row.names=FALSE)
