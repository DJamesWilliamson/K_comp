# Random forest models on different imputed sets and selected variables

# Function to check accuracy of a 2x2 confusion matrix
checkAccuracy2x2 = function(x) {
        accuracy = (x[1,1]+x[2,2])/(x[1,1]+x[1,2]+x[2,1]+x[2,2])
        print(accuracy)
}

# Import imputed sets
# train_mF_imp2 <- read.csv("train_mF_imp2.csv")
# test_mF_imp2 <- read.csv("test_mF_imp2.csv")

# Examine datasets
str(train_mF_imp2)
str(test_mF_imp2)
names(train_mF_imp2)
names(test_mF_imp2)
levels(test_mF_imp2$YOB)
# "(1928,1957]" "(1957,1967]" "(1967,1973]" "(1973,1978]" "(1978,1983]" "(1983,1987]" "(1987,1991]" "(1991,1994]"
# "(1994,1996]" "(1996,2001]"
train_mF_imp2$Party <- train_mF_imp2[ ,"train2016$Party"]
train_mF_imp2[ ,"train2016$Party"] <- NULL
train <- train_mF_imp2
test <- test_mF_imp2

# Set up split of train for internal use
library(caTools)
set.seed(88)
split = sample.split(train$Party, SplitRatio = 0.75)
inTrain = subset(train, split == TRUE)
inTest = subset(train, split == FALSE)

# Select covariates for model (0.6091954 internal)
covs <- cov_frequent

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
library(randomForest)
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = inTrain)
rf_predict = predict(fr_model, newdata = inTest)
conf_matrix = table(inTest$Party, rf_predict)
checkAccuracy2x2(conf_matrix)

########################################################################

# Select covariates for model (0.6034483 internal)
covs <- c(nonq_covs, sigQs)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = inTrain)
rf_predict = predict(rf_model, newdata = inTest)
conf_matrix = table(inTest$Party, rf_predict)
checkAccuracy2x2(conf_matrix)

##########################################################################

# Read in other covariate groups:

cov_consensus <- read.csv("cov_consensus.txt", stringsAsFactors = FALSE, header = FALSE)
str(cov_consensus)
cov_consensus <- cov_consensus$V1

# Select covariates for model (0.6063218 internal)
covs <- cov_consensus

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = inTrain)
rf_predict = predict(rf_model, newdata = inTest)
conf_matrix = table(inTest$Party, rf_predict)
checkAccuracy2x2(conf_matrix)

################################################################################
# Try all covariates!

# Select covariates for model (0.6106322 internal)
covs <- names(test)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = inTrain)
rf_predict = predict(rf_model, newdata = inTest)
conf_matrix = table(inTest$Party, rf_predict)
checkAccuracy2x2(conf_matrix)

################################################################################
################################################################################

# Try models for submission
train <- train_mF_imp2
test <- test_mF_imp2

# Select covariates for model (0.6106322 internal; mistake external)
covs <- names(test)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = train)
rf_predict = predict(rf_model, newdata = test)

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf1 = data.frame(USER_ID = test2016$USER_ID, Predictions = as.character(rf_predict))
write.csv(Sub_rf1, "Sub_rf1.csv", row.names=FALSE)

#####################################################################
# Select covariates for model (0.6063218 internal, external mistake)
covs <- cov_consensus

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = train)
rf_predict = predict(rf_model, newdata = test)

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf2 = data.frame(USER_ID = test2016$USER_ID, Predictions = as.character(rf_predict))
write.csv(Sub_rf2, "Sub_rf2.csv", row.names=FALSE)

###############################################################################
###############################################################################

# Try with different imputed set

train <- read.csv("train_imp1_set1.csv")
test <- read.csv("test_imp1_set1.csv")
str(train)
str(test)

# Change name of train Party variable; and include USER_ID
# train2016 <- read.csv("train2016.csv", na.strings = c("", "NA"))
# test2016 <- read.csv("test2016.csv", na.strings = c("", "NA"))
# str(train2016)
# str(test2016)

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
# Select covariates for model (external 0.616)
covs <- cov_consensus

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = train)
rf_predict = predict(rf_model, newdata = test)

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf3 = data.frame(USER_ID = test2016$USER_ID, Predictions = as.character(rf_predict))
write.csv(Sub_rf3, "Sub_rf3.csv", row.names=FALSE)

################################################################################
# Select covariates for model ()
covs <- cov_frequent

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = train)
rf_predict = predict(rf_model, newdata = test)

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf4 = data.frame(USER_ID = test2016$USER_ID, Predictions = as.character(rf_predict))
write.csv(Sub_rf4, "Sub_rf4.csv", row.names=FALSE)
################################################################################
# Back to rf imputation and rf1 in whose submission there was a mistake before

train <- train_mF_imp2
test <- test_mF_imp2

# Select covariates for model
covs <- names(test)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = train)
rf_predict = predict(rf_model, newdata = test)

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf5 = data.frame(USER_ID = test2016$USER_ID, Predictions = as.character(rf_predict))
write.csv(Sub_rf5, "Sub_rf5.csv", row.names=FALSE)
Sub_rf5

# rf7 uses the imp3_mode dataset
#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf7 = data.frame(USER_ID = test2016$USER_ID, Predictions = as.character(rf_predict))
write.csv(Sub_rf7, "Sub_rf7.csv", row.names=FALSE)
Sub_rf7


################################################################################
################################################################################
# Trial of random forest model with mode of imp3 (20 iterations in mice)

train <- train_imp3_mode
test <- test_imp3_mode

# Select covariates for model
covs <- names(test)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = randomForest(as.formula(model_formula), data = train)
rf_predict = predict(rf_model, newdata = test)

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf6 = data.frame(USER_ID = test2016$USER_ID, Predictions = as.character(rf_predict))
write.csv(Sub_rf6, "Sub_rf6.csv", row.names=FALSE)
Sub_rf6

#################################################################################
# Use of cForest from party package with imp3_mode imputed dataset (follows gbm model)

library(party)

head(train)
head(test)

# Select covariates for model (all used here)
covs <- names(test)[-1]

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = cforest(as.formula(model_formula), data = train)  # control = cforest_unbiased(ntree = 2000, mtry = 2)
rf_predict = predict(rf_model, newdata = test, type = "prob")
rf_predict <- unlist(rf_predict)
rf_predict <- ifelse(rf_predict < 0.5, "Democrat", "Republican")

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf7 = data.frame(USER_ID = test2016$USER_ID, Predictions = rf_predict)
write.csv(Sub_rf7, "Sub_rf7.csv", row.names=FALSE)
Sub_rf7
#################################################################################
# Repeat of above with additional control settings

set.seed(22)
rf_model = cforest(as.formula(model_formula), data = train, controls = cforest_unbiased(ntree = 2000, mtry = 3))
rf_predict = predict(rf_model, newdata = test, type = "prob")
rf_predict <- unlist(rf_predict)
rf_predict <- ifelse(rf_predict < 0.5, "Democrat", "Republican")
prop.table(table(rf_predict))   # 66% Democrat
#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf8 = data.frame(USER_ID = test2016$USER_ID, Predictions = rf_predict)
write.csv(Sub_rf8, "Sub_rf8.csv", row.names=FALSE)
Sub_rf8

#################################################################################
# Reduced covariates

# Select covariates for model (all used here)
covs <- c(cov_sigQplus, "Income", "EducationLevel")

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Build random forest
set.seed(22)
rf_model = cforest(as.formula(model_formula), data = train, controls = cforest_unbiased(ntree = 2000, mtry = 2))
rf_predict = predict(rf_model, newdata = test, type = "prob")
rf_predict <- unlist(rf_predict)
rf_predict <- ifelse(rf_predict < 0.49, "Democrat", "Republican")
prop.table(table(rf_predict))   # 51% Democrat @ 0.5; 46% @ 0.49
#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_rf9 = data.frame(USER_ID = test2016$USER_ID, Predictions = rf_predict)
write.csv(Sub_rf9, "Sub_rf9.csv", row.names=FALSE)
Sub_rf9
