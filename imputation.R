#################################################################################
# Import training set but replace blanks with NA
trainMi <- read.csv("train2016.csv", na.strings = c("", "NA"))
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
