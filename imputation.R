#################################################################################
# Imputation 1: uses data from both training and test datasets, no additional feature

# Import training and test sets but replace blanks with NA
train2016 <- read.csv("train2016.csv", na.strings = c("", "NA"))
test2016 <- read.csv("test2016.csv", na.strings = c("", "NA"))
str(train2016)
str(test2016)
names(train2016)[1:8]
names(test2016)[1:8]

# Remove the ID and response variables from train2016
# trainMi <- train2016[ , -c(1, 7)]
# testMi <- test2016[ , -c(1)]
# names(trainMi)[1:8]
# names(testMi)[1:8]

# OR
vars.for.imputation = setdiff(names(train2016), c("USER_ID", "Party"))
trainMi <- train2016[ , vars.for.imputation]
testMi <- test2016[ , vars.for.imputation]

# Combine train and test datasets (and remove others from workspace)
impData <- rbind(trainMi, testMi)
rm(train2016, test2016, trainMi, testMi)
summary(impData)

# Set to NA cases with  10 < age < 100 (for later imputation)
impData$YOB[which(2013 - impData$YOB > 100)] <- NA
impData$YOB[which(2013 - impData$YOB < 11)] <- NA

# Replace YOB with factor variable in deciles of YOB; plans to reduce errors
impData$YOB = cut(impData$YOB, quantile(impData$YOB, prob=seq(0, 1, length=11), 
                                        na.rm = TRUE, type = 5), ordered_result = TRUE)

# Order other factors
impData$Income <- ordered(impData$Income,
                          levels = c("under $25,000", "$25,001 - $50,000",
                                     "$50,000 - $74,999", "$75,000 - $100,000",
                                     "$100,001 - $150,000", "over $150,000"))
impData$EducationLevel <- ordered(impData$EducationLevel,
                                  levels = c("Current K-12", "High School Diploma",
                                             "Current Undergraduate", "Associate's Degree",
                                             "Bachelor's Degree", "Master's Degree",
                                             "Doctoral Degree"))
str(impData)
################################################################################
# Imputation preparation
library(mice)
library(VIM)
md.pattern(impData)

# tail(md.pattern(impData)[ ,1:8])
# marginplot(impData[, c("EducationLevel", "Income")], col = mdc(1:2), cex = 1.2,
#            cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# Proportion of usable cases
p <- md.pairs(impData)
use_cases <- round(p$mr/(p$mr + p$mm), 3)
head(use_cases)
class(use_cases)
mean(use_cases < 0.1)

# Create default prediction matrix and examine the distribution of variables used
pred_quick <- quickpred(impData)
hist(rowSums(pred_quick))

# Export pred_quick
write.csv(as.data.frame(pred_quick), file = "pred_quick_imp1.csv")

# This is combined with the question matrix I derived from the question selection
# procedure so that the non-Q predictions come from here and the Q prediction matrix
# comes from the question selections

# Import the combined prediction data and remove the first column, name rows, create matrix
pred <- read.csv("pred_quick_imp1_q_comb.csv", header = TRUE, stringsAsFactors = FALSE)
pred <- pred[ , -c(1)]
colnames(pred)
rownames(pred) <- colnames(pred)
pred <- as.matrix(pred)

# Questions not used for imputation 1
which(colSums(pred) == 0)
# Do you find it easier to start and maintain a new good habit, or to permanently kick a bad habit?
# Is your alarm clock intentionally set to be a few minutes fast?
# Are you left-handed?

# Number or predictor variables used for each imputation
hist(rowSums(pred))
which(rowSums(pred) < 10)       # These are the ones (37 and 94) from before
# Do you find it easier to start and maintain a new good habit, or to permanently kick a bad habit?
# Are you left-handed?
# Could add the predictors from pred_quick to the 6 I identified, but not worth it

################################################################################
# Trial imputation 1 (~5 mins)
imp1 <- mice(impData, pred = pred, seed = 23109)

# Check the prediction matrix and sets
print(imp1)
imp1$imp$YOB             # some variability in the imputations between runs
head(complete(imp1))     # look at first complete set
head(complete(imp1, 2))  # look at the others complete set

imp1_set1 <- complete(imp1)
imp1_set2 <- complete(imp1, 2)
imp1_set3 <- complete(imp1, 3)
imp1_set4 <- complete(imp1, 4)
imp1_set5 <- complete(imp1, 5)

write.csv(imp1_set1, "imp1_set1.csv")
write.csv(imp1_set2, "imp1_set2.csv")
write.csv(imp1_set3, "imp1_set3.csv")
write.csv(imp1_set4, "imp1_set4.csv")
write.csv(imp1_set5, "imp1_set5.csv")

par(mfrow = c(3, 1))
plot(imp1, c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel"))
plot(imp1, c(names(colnames(pred)[5:length(colnames(pred))])))
par(mfrow = c(1, 1))
# May be some problems with convergence with Q102687 and Q108856; others not ideal
# Do you eat breakfast every day?
# Lots of people are around! Are you more likely to be right in the middle of things, 
# or looking for your own quieter space?


# One can also do a dry run to set the default parameters
# ini <- mice(impData, pred = pred, seed = 23109, maxit = 0)

################################################################################
# Trial imputation 2 (with 10 iterations, default is 5; ~15 mins)
imp2 <- mice(impData, pred = pred, seed = 23109, maxit = 10)

# Check for convergence
par(mfrow = c(3, 1))
plot(imp2, c(names(colnames(pred))))
par(mfrow = c(1, 1))

################################################################################
# Trial imputation 3 (with 20 iterations, default is 5; ~35 mins)
imp3 <- mice(impData, pred = pred, seed = 23109, maxit = 20, print = TRUE)

# Check for convergence which look to be reasonable
par(mfrow = c(3, 1))
plot(imp3, c(names(colnames(pred))))
par(mfrow = c(1, 1))

imp3_set1 <- complete(imp3)
imp3_set2 <- complete(imp3, 2)
imp3_set3 <- complete(imp3, 3)
imp3_set4 <- complete(imp3, 4)
imp3_set5 <- complete(imp3, 5)

write.csv(imp3_set1, "imp3_set1.csv")
write.csv(imp3_set2, "imp3_set2.csv")
write.csv(imp3_set3, "imp3_set3.csv")
write.csv(imp3_set4, "imp3_set4.csv")
write.csv(imp3_set5, "imp3_set5.csv")

# Re-create the imputed train and test sets:
# from imp1_set1
train_imp1_set1 <- cbind(imp1_set1[1:nrow(train2016), ], train2016$Party)
# names(train_imp1_set1)
# all.equal(nrow(train_imp1_set1), nrow(train2016))
test_imp1_set1 <- cbind(imp1_set1[(nrow(train2016)+1):nrow(imp1_set1), ])
# names(test_imp1_set1)
# all.equal(nrow(test_imp1_set1), nrow(test2016))
write.csv(train_imp1_set1, "train_imp1_set1.csv")
write.csv(test_imp1_set1, "test_imp1_set1.csv")


# from imp3_set1 to set5
train_imp3_set1 <- cbind(imp3_set1[1:nrow(train2016), ], train2016$Party)
test_imp3_set1 <- cbind(imp3_set1[(nrow(train2016)+1):nrow(imp3_set1), ])

train_imp3_set2 <- cbind(imp3_set2[1:nrow(train2016), ], train2016$Party)
test_imp3_set2 <- cbind(imp3_set2[(nrow(train2016)+1):nrow(imp3_set2), ])

train_imp3_set3 <- cbind(imp3_set3[1:nrow(train2016), ], train2016$Party)
test_imp3_set3 <- cbind(imp3_set3[(nrow(train2016)+1):nrow(imp3_set3), ])

train_imp3_set4 <- cbind(imp3_set4[1:nrow(train2016), ], train2016$Party)
test_imp3_set4 <- cbind(imp3_set4[(nrow(train2016)+1):nrow(imp3_set4), ])

train_imp3_set5 <- cbind(imp3_set5[1:nrow(train2016), ], train2016$Party)
test_imp3_set5 <- cbind(imp3_set5[(nrow(train2016)+1):nrow(imp3_set5), ])

write.csv(train_imp3_set1, "train_imp3_set1.csv")
write.csv(test_imp3_set1, "test_imp3_set1.csv")

write.csv(train_imp3_set2, "train_imp3_set2.csv")
write.csv(test_imp3_set2, "test_imp3_set2.csv")

write.csv(train_imp3_set3, "train_imp3_set3.csv")
write.csv(test_imp3_set3, "test_imp3_set3.csv")

write.csv(train_imp3_set4, "train_imp3_set4.csv")
write.csv(test_imp3_set4, "test_imp3_set4.csv")

write.csv(train_imp3_set5, "train_imp3_set5.csv")
write.csv(test_imp3_set5, "test_imp3_set5.csv")
################################################################################
################################################################################
# Trial imputation 4 with only train dataset (ie didn't combine with test)

impData <- trainMi
# Set to NA cases with  10 < age < 100 (for later imputation)
impData$YOB[which(2013 - impData$YOB > 100)] <- NA
impData$YOB[which(2013 - impData$YOB < 11)] <- NA
# Replace YOB with factor variable in deciles of YOB; plans to reduce errors
impData$YOB = cut(impData$YOB, quantile(impData$YOB, prob=seq(0, 1, length=11), 
                                        na.rm = TRUE, type = 5), ordered_result = TRUE)
# Order other factors
impData$Income <- ordered(impData$Income,
                          levels = c("under $25,000", "$25,001 - $50,000",
                                     "$50,000 - $74,999", "$75,000 - $100,000",
                                     "$100,001 - $150,000", "over $150,000"))
impData$EducationLevel <- ordered(impData$EducationLevel,
                                  levels = c("Current K-12", "High School Diploma",
                                             "Current Undergraduate", "Associate's Degree",
                                             "Bachelor's Degree", "Master's Degree",
                                             "Doctoral Degree"))
str(impData)
# Proportion of usable cases
p <- md.pairs(impData)
use_cases <- round(p$mr/(p$mr + p$mm), 3)
head(use_cases)
class(use_cases)
mean(use_cases < 0.1)

# Create default prediction matrix and examine the distribution of variables used
pred_quick <- quickpred(impData, minpuc = 0.1)
hist(rowSums(pred_quick), breaks = 20)
colnames(pred_quick)[rowSums(pred_quick) <3]  # 10 variables have <3 predictors
colnames(pred_quick)[rowSums(pred_quick) == 0]

# Export pred_quick
write.csv(as.data.frame(pred_quick), file = "pred_quick_imp4.csv")

# This is combined with the question matrix I derived from the question selection
# procedure so that the non-Q predictions come from here and the Q prediction matrix
# comes from the question selections combined with this matrix

# Import the combined prediction data and remove the first column, name rows, create matrix
pred <- read.csv("pred_quick_imp4_q_comb.csv", header = TRUE, stringsAsFactors = FALSE)
# head(pred)
pred <- pred[ , -c(1)]
colnames(pred)
rownames(pred) <- colnames(pred)
pred <- as.matrix(pred)

hist(rowSums(pred), breaks = 50)
# Most have 15 - 20 predictors
which(rowSums(pred) < 10) 
# Do you find it easier to start and maintain a new good habit, or to permanently kick a bad habit?

# Run imputation, this time using 30 iterations (but in 3 steps to save RAM and check convergence)
imp4 <- mice(impData, pred = pred, seed = 23109, maxit = 10, print = TRUE)
# Check for convergence
par(mfrow = c(3, 1))
plot(imp4, c(names(colnames(pred))))
par(mfrow = c(1, 1))
# Run further 10 iterations, don't reset seed
imp4 <- mice.mids(imp4, pred = pred, maxit = 10, print = TRUE)
# Check for convergence
par(mfrow = c(3, 1))
plot(imp4, c(names(colnames(pred))))
par(mfrow = c(1, 1))
# Run further 10 iterations, don't reset seed
imp4 <- mice.mids(imp4, pred = pred, maxit = 10, print = TRUE)
# Check for convergence
par(mfrow = c(3, 1))
plot(imp4, c(names(colnames(pred))))
par(mfrow = c(1, 1))
untouched <- imp4


# Unsure how to add back the outcome variable, Party, to all the datasets!
summary(imp4)
print(imp4)
# plot(imp4)
complete(imp4, 1)


################################################################################
################################################################################
library(missForest)
# Import training and test sets but replace blanks with NA
train2016 <- read.csv("train2016.csv", na.strings = c("", "NA"))
test2016 <- read.csv("test2016.csv", na.strings = c("", "NA"))

vars.for.imputation = setdiff(names(train2016), c("USER_ID", "Party"))
trainMi <- train2016[ , vars.for.imputation]
testMi <- test2016[ , vars.for.imputation]
# Combine train and test datasets
impData <- rbind(trainMi, testMi)
str(impData)
summary(impData)
# Set to NA cases with  10 < age < 100 (for later imputation)
impData$YOB[which(2013 - impData$YOB > 100)] <- NA
impData$YOB[which(2013 - impData$YOB < 11)] <- NA
# Replace YOB with factor variable in deciles of YOB; plans to reduce errors
impData$YOB = cut(impData$YOB, quantile(impData$YOB, prob=seq(0, 1, length=11), 
                                        na.rm = TRUE, type = 5), ordered_result = TRUE)
# Order other factors
impData$Income <- ordered(impData$Income,
                          levels = c("under $25,000", "$25,001 - $50,000",
                                     "$50,000 - $74,999", "$75,000 - $100,000",
                                     "$100,001 - $150,000", "over $150,000"))
impData$EducationLevel <- ordered(impData$EducationLevel,
                                  levels = c("Current K-12", "High School Diploma",
                                             "Current Undergraduate", "Associate's Degree",
                                             "Bachelor's Degree", "Master's Degree",
                                             "Doctoral Degree"))
str(impData)
# Impute training set using missForest
set.seed(5555)
mF_impData <- missForest(impData, variablewise = TRUE, verbose = TRUE)
# Review the error rate (ie the proportion of falsely classified entries
# (PFC) in the categorical part of the imputed data set,)
mF_impData_error <- mF_train$OOBerror
mF_impData_error
# Review the imputed data
mF_impData$ximp
str(mF_impData$ximp)
# Save output
write.csv(mF_impData$ximp, "mF_impData.csv", row.names=FALSE)
#Keep levels of YOB in case required
levels(impData$YOB)
write.csv(levels(impData$YOB), "mF_impData_YOBlevels.csv", row.names=FALSE)

# Re-create the imputed train and test sets:
train_mF_imp2 <- cbind(mF_impData$ximp[1:nrow(train2016), ], train2016$Party)
test_mF_imp2 <- cbind(mF_impData$ximp[(nrow(train2016)+1):nrow(mF_impData$ximp), ])
write.csv(train_mF_imp2, "train_mF_imp2.csv")
write.csv(test_mF_imp2, "test_mF_imp2.csv")

################################################################################
################################################################################



























# Imputation with the missForest package (see below: imputation of train and test
# separately resulted in different ordinal break points of the YOB variable)
library(missForest)
# Trial of this package on the train and test data; note verbose = TRUE
impData_train <- trainMi
impData <- trainMi
# Set to NA cases with  10 < age < 100 (for later imputation)
impData$YOB[which(2013 - impData$YOB > 100)] <- NA
impData$YOB[which(2013 - impData$YOB < 11)] <- NA
# Replace YOB with factor variable in deciles of YOB; plans to reduce errors
impData$YOB = cut(impData$YOB, quantile(impData$YOB, prob=seq(0, 1, length=11), 
                                        na.rm = TRUE, type = 5), ordered_result = TRUE)
# Order other factors
impData$Income <- ordered(impData$Income,
                          levels = c("under $25,000", "$25,001 - $50,000",
                                     "$50,000 - $74,999", "$75,000 - $100,000",
                                     "$100,001 - $150,000", "over $150,000"))
impData$EducationLevel <- ordered(impData$EducationLevel,
                                  levels = c("Current K-12", "High School Diploma",
                                             "Current Undergraduate", "Associate's Degree",
                                             "Bachelor's Degree", "Master's Degree",
                                             "Doctoral Degree"))
str(impData)
train_data <- impData
str(train_data)
# Impute training set using missForest
# set.seed()
mF_train <- missForest(train_data, variablewise = TRUE, verbose = TRUE)
# Review the imputed data
mF_train$ximp
write.csv(mF_train$ximp, "mF_train.csv", row.names=FALSE)
# Review the error rate (ie the proportion of falsely classified entries
# (PFC) in the categorical part of the imputed data set,)
mF_train_error <- mF_train$OOBerror

############################################
# Repeat for test data
impData_test <- testMi
impData <- testMi
# Set to NA cases with  10 < age < 100 (for later imputation)
impData$YOB[which(2013 - impData$YOB > 100)] <- NA
impData$YOB[which(2013 - impData$YOB < 11)] <- NA
# Replace YOB with factor variable in deciles of YOB; plans to reduce errors
impData$YOB = cut(impData$YOB, quantile(impData$YOB, prob=seq(0, 1, length=11), 
                                        na.rm = TRUE, type = 5), ordered_result = TRUE)
# Order other factors
impData$Income <- ordered(impData$Income,
                          levels = c("under $25,000", "$25,001 - $50,000",
                                     "$50,000 - $74,999", "$75,000 - $100,000",
                                     "$100,001 - $150,000", "over $150,000"))
impData$EducationLevel <- ordered(impData$EducationLevel,
                                  levels = c("Current K-12", "High School Diploma",
                                             "Current Undergraduate", "Associate's Degree",
                                             "Bachelor's Degree", "Master's Degree",
                                             "Doctoral Degree"))
str(impData)
test_data <- impData
str(test_data) 
# Impute training set using missForest
# set.seed()
mF_test <- missForest(test_data, variablewise = TRUE, verbose = TRUE)
# Review the imputed data
mF_test$ximp[1]
write.csv(mF_test$ximp, "mF_test.csv", row.names=FALSE)
# Review the error rate (ie the proportion of falsely classified entries
# (PFC) in the categorical part of the imputed data set,)
mF_test_error <- mF_test$OOBerror

################################################################################
################################################################################
# Imputation with the missForest package (this time combining train and test)


















































# Preliminary data exploration



str(trainMi)
summary(trainMi)

hist(trainMi$YOB)
hist(2013 - trainMi$YOB)
max(2013 - trainMi$YOB, na.rm = TRUE)

sum(2013 - trainMi$YOB > 100, na.rm = TRUE)
sum(2013 - trainMi$YOB < 11, na.rm = TRUE)

# Very unlikely combinations of age and income, but similar data expected in test
# Decided to impute (by setting to NA) cases with  10 < age < 100
trainMi$YOB[which(2013 - trainMi$YOB > 100)] <- NA
trainMi$YOB[which(2013 - trainMi$YOB < 11)] <- NA

# Replace YOB with factor variable in deciles of YOB; plans to reduce errors
trainMi$YOB = cut(trainMi$YOB, quantile(trainMi$YOB, prob=seq(0, 1, length=11), 
                                         na.rm = TRUE, type = 5), ordered_result = TRUE)

# Replace Party with missingValues and create ordered factor
trainMi$Party <- rowSums(is.na(trainMi))
names(trainMi)[7] <- "missingValues"
hist(trainMi$missingValues, breaks = 100)
# trainMi$missingValues = cut(trainMi$missingValues, quantile(trainMi$missingValues, 
#                                 prob=seq(0, 1, length=6), na.rm = TRUE, type = 5))

trainMi$missingValues <- cut(trainMi$missingValues, breaks = c(0, 10, 70, 105),
                             labels = c("low", "medium", "high"),  na.rm = FALSE,
                             ordered_result = TRUE, include.lowest = TRUE)
plot(trainMi$missingValues)

# Order other factors
levels(trainMi$Income)
head(levels(trainMi$Income))
trainMi$Income <- ordered(trainMi$Income,
                          levels = c("under $25,000", "$25,001 - $50,000",
                                        "$50,000 - $74,999", "$75,000 - $100,000",
                                        "$100,001 - $150,000", "over $150,000"))
trainMi$EducationLevel <- ordered(trainMi$EducationLevel,
                                levels = c("Current K-12", "High School Diploma",
                                           "Current Undergraduate", "Associate's Degree",
                                           "Bachelor's Degree", "Master's Degree",
                                           "Doctoral Degree"))

str(trainMi)

library(mice)
library(VIM)
md.pattern(trainMi)
tail(md.pattern(trainMi)[ ,1:8])
marginplot(trainMi[, c("EducationLevel", "Income")], col = mdc(1:2), cex = 1.2,
                                cex.lab = 1.2, cex.numbers = 1.3, pch = 19)

# Trial imputation
trial_set <- trainMi[ ,2:7]
head(trial_set)
md.pattern(trial_set)
imp <- mice(trial_set, seed = 23109)
# Check
print(imp)
imp$imp$YOB             # some variability in the imputations between runs
head(complete(imp))     # look at first complete set
head(complete(imp, 2))  # look at second complete set
library(lattice)
stripplot(imp, pch = 20, cex = 1.2)     # not working
xyplot(imp, EducationLevel ~ Income | .imp, pch = 20, cex = 1.4) # not useful
# Run model
fit <- with(imp, glm(Income ~ EducationLevel + YOB, family = "binomial"))
print(pool(fit))
round(summary(pool(fit)), 2)
