# Trial of glmulti package see:
# http://stackoverflow.com/questions/10182804/using-the-glmulti-package-in-r-for-exhaustive-search-multiple-regression-for-aka
library(glmulti)

# Import/allocate imputed data
train <- train_imp3_mode
test <- test_imp3_mode

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
str(train)
str(test)

# Set up split of train for internal use
library(caTools)
set.seed(88)
split = sample.split(train$Party, SplitRatio = 0.75)
inTrain = subset(train, split == TRUE)
inTest = subset(train, split == FALSE)

# Read in covariates
cov_consensus <- read.csv("cov_consensus.txt", stringsAsFactors = FALSE, header = FALSE)
str(cov_consensus)
cov_consensus <- cov_consensus$V1

cov_frequent <- read.csv("cov_frequent.txt", stringsAsFactors = FALSE, header = FALSE)
str(cov_frequent)
cov_frequent <- cov_frequent$V1

cov_optimum <- read.csv("cov_optimum.txt", stringsAsFactors = FALSE, header = FALSE)
str(cov_optimum)
cov_optimum <- cov_optimum$V1

################################################################################
# Select covariates for model
covs <- cov_optimum

# Set subset for glmulti
glmulti_Traindata <- inTrain[, c("Party", cov_optimum)]

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ")
model_formula <- c("Party", covs)
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
glmulti.logistic.out <-
        glmulti(as.formula(model_formula), data = glmulti_Traindata,
                level = 1,               # No interaction considered
                method = "h",            # Exhaustive approach
                crit = "aic",            # AIC as criteria
                confsetsize = 5,         # Keep 5 best models
                plotty = F, report = F,  # No plot or interim reports
                fitfunction = "glm",     # glm function
                family = binomial)       # binomial family for logistic regression

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.logistic.out@formulas
# [[1]]
# Party ~ 1 + Q102089 + Q109244 + Q113181 + Q115611 + Q115899 +
#         Q116881 + Q121699 + Q98869
# <environment: 0x000000001330f2b0>
#         
#         [[2]]
# Party ~ 1 + Q109244 + Q113181 + Q115611 + Q115899 + Q116881 + 
#         Q121699 + Q98869
# <environment: 0x000000001330f2b0>
#         
#         [[3]]
# Party ~ 1 + Q102089 + Q109244 + Q113181 + Q115611 + Q115899 + 
#         Q116881 + Q116953 + Q121699 + Q98869
# <environment: 0x000000001330f2b0>
#         
#         [[4]]
# Party ~ 1 + Q109244 + Q113181 + Q115611 + Q115899 + Q116881 + 
#         Q116953 + Q121699 + Q98869
# <environment: 0x000000001330f2b0>
#         
#         [[5]]
# Party ~ 1 + Q106272 + Q109244 + Q113181 + Q115611 + Q115899 + 
#         Q116881 + Q121699 + Q98869
# <environment: 0x000000001330f2b0>
        
## Show result for the best model
summary(glmulti.logistic.out@objects[[1]])
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -0.37539    0.11330  -3.313 0.000922 ***
#         Q102089Rent  -0.10589    0.07231  -1.464 0.143102    
#         Q109244Yes   -1.05593    0.08489 -12.438  < 2e-16 ***
#         Q113181Yes    0.35832    0.07119   5.033 4.82e-07 ***
#         Q115611Yes    0.53491    0.07179   7.451 9.26e-14 ***
#         Q115899Me     0.15100    0.06671   2.263 0.023612 *  
#         Q116881Right  0.21038    0.07506   2.803 0.005067 ** 
#         Q121699Yes   -0.18278    0.07745  -2.360 0.018279 *  
#         Q98869Yes     0.23613    0.09161   2.578 0.009949 ** 
# AIC: 5377.2

################################################################################

# Select covariates for model
covs <- cov_frequent

# Set subset for glmulti
glmulti_Traindata <- inTrain[, c("Party", cov_frequent)]

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ")
model_formula <- c("Party", covs)
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model (took>2.5h)
glmulti.logistic.out <-
        glmulti(as.formula(model_formula), data = glmulti_Traindata,
                level = 1,               # No interaction considered
                method = "h",            # Exhaustive approach
                crit = "aic",            # AIC as criteria
                confsetsize = 5,         # Keep 5 best models
                plotty = F, report = F,  # No plot or interim reports
                fitfunction = "glm",     # glm function
                family = binomial)       # binomial family for logistic regression

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.logistic.out@formulas
# [[1]]
# Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 +
#         Q113181 + Q115611 + Q115899 + Q116881 + Q120379 + Q121699 +
#         Q98869 + Q99480 + YOB
# <environment: 0x0000000019106670>
#         
#         [[2]]
# Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 + 
#         Q113181 + Q115611 + Q115899 + Q116881 + Q120379 + Q98869 + 
#         Q99480 + YOB
# <environment: 0x0000000019106670>
#         
#         [[3]]
# Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 + 
#         Q113181 + Q115611 + Q115899 + Q116881 + Q121699 + Q98869 + 
#         Q99480 + YOB
# <environment: 0x0000000019106670>
#         
#         [[4]]
# Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 + 
#         Q113181 + Q115611 + Q115899 + Q116881 + Q98869 + Q99480 + 
#         YOB
# <environment: 0x0000000019106670>
#         
#         [[5]]
# Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 + 
#         Q113181 + Q115611 + Q115899 + Q116881 + Q120379 + Q120472 + 
#         Q98869 + Q99480 + YOB
# <environment: 0x0000000019106670>
#         
## Show result for the best model
summary(glmulti.logistic.out@objects[[1]])

# Coefficients:
#                                               Estimate Std. Error z value Pr(>|z|)    
#         (Intercept)                               -1.26006    0.25376  -4.966 6.85e-07 ***
#         HouseholdStatusDomestic Partners (w/kids)  0.39546    0.40024   0.988 0.323120    
#         HouseholdStatusMarried (no kids)           0.62682    0.22871   2.741 0.006132 ** 
#         HouseholdStatusMarried (w/kids)            0.85637    0.21774   3.933 8.39e-05 ***
#         HouseholdStatusSingle (no kids)            0.61001    0.21498   2.838 0.004546 ** 
#         HouseholdStatusSingle (w/kids)             0.53932    0.26673   2.022 0.043184 *  
#         Q101163Mom                                -0.18762    0.06748  -2.780 0.005429 ** 
#         Q102089Rent                               -0.15989    0.08371  -1.910 0.056131 .  
#         Q109244Yes                                -0.96983    0.08733 -11.105  < 2e-16 ***
#         Q110740PC                                  0.14066    0.06771   2.077 0.037759 *  
#         Q113181Yes                                 0.34494    0.07297   4.727 2.27e-06 ***
#         Q115611Yes                                 0.49795    0.07577   6.572 4.97e-11 ***
#         Q115899Me                                  0.15127    0.06786   2.229 0.025809 *  
#         Q116881Right                               0.18251    0.07630   2.392 0.016754 *  
#         Q120379Yes                                -0.10163    0.07081  -1.435 0.151197    
#         Q121699Yes                                -0.12493    0.08754  -1.427 0.153564    
#         Q98869Yes                                  0.20591    0.09273   2.220 0.026388 *  
#         Q99480Yes                                  0.32936    0.08909   3.697 0.000218 ***
#         YOB.L                                      0.56584    0.15255   3.709 0.000208 ***
#         YOB.Q                                     -0.09713    0.12022  -0.808 0.419117    
#         YOB.C                                     -0.04698    0.11131  -0.422 0.672987    
#         YOB^4                                      0.01269    0.10475   0.121 0.903570    
#         YOB^5                                      0.25091    0.10576   2.372 0.017672 *  
#         YOB^6                                     -0.03010    0.10383  -0.290 0.771862    
#         YOB^7                                     -0.11979    0.10440  -1.147 0.251229    
#         YOB^8                                      0.20062    0.10647   1.884 0.059527 .  
#         YOB^9                                     -0.07669    0.10517  -0.729 0.465884    
# AIC: 5347.7

################################################################################
# Function to check accuracy of a 2x2 confusion matrix
checkAccuracy2x2 = function(x) {
        accuracy = (x[1,1]+x[2,2])/(x[1,1]+x[1,2]+x[2,1]+x[2,2])
        print(accuracy)
}

################################################################################
# Try some models internally against inTest

# Run model (glm13 best fit from frequent covs)
model_glm13 <- glm(Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 +
                          Q113181 + Q115611 + Q115899 + Q116881 + Q120379 + Q121699 +
                          Q98869 + Q99480 + YOB, data = inTrain, family = "binomial")
summary(model_glm13)
model_glm <- model_glm13

PredTest = predict(model_glm, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm13 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

conf_matrix = table(inTest$Party, PredTestLabels_glm13)
checkAccuracy2x2(conf_matrix)

summary(model_glm)$call
summary(model_glm)$aic

################################################################################

# Run model (glm14 second best fit from frequent covs)
model_glm14 <- glm(Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 + 
                           Q113181 + Q115611 + Q115899 + Q116881 + Q120379 + Q98869 + 
                           Q99480 + YOB, data = inTrain, family = "binomial")
summary(model_glm14)
model_glm <- model_glm14

PredTest = predict(model_glm, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm14 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

conf_matrix = table(inTest$Party, PredTestLabels_glm14)
checkAccuracy2x2(conf_matrix)

summary(model_glm)$call
summary(model_glm)$aic

################################################################################

# Run model (glm15  best fit from optimum covs)
model_glm15 <- glm(Party ~ 1 + Q102089 + Q109244 + Q113181 + Q115611 + Q115899 +
                           Q116881 + Q121699 + Q98869, data = inTrain, family = "binomial")
summary(model_glm15)
model_glm <- model_glm15

PredTest = predict(model_glm, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm15 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

conf_matrix = table(inTest$Party, PredTestLabels_glm15)
checkAccuracy2x2(conf_matrix)

summary(model_glm)$call
summary(model_glm)$aic

################################################################################
# Run model (glm13 best fit from frequent covs, 0.64 internal, 0.59 external)
model_glm13 <- glm(Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 +
                           Q113181 + Q115611 + Q115899 + Q116881 + Q120379 + Q121699 +
                           Q98869 + Q99480 + YOB, data = train, family = "binomial")
summary(model_glm13)
model_glm <- model_glm13

PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels_glm13 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm13 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels_glm13)
write.csv(Sub_glm13, "Sub_glm13.csv", row.names=FALSE)
################################################################################
# Run model (glm14 second best fit from frequent covs, 0.64 internal, 0.59 external)
model_glm14 <- glm(Party ~ 1 + HouseholdStatus + Q101163 + Q102089 + Q109244 + Q110740 + 
                           Q113181 + Q115611 + Q115899 + Q116881 + Q120379 + Q98869 + 
                           Q99480 + YOB, data = train, family = "binomial")
summary(model_glm14)
model_glm <- model_glm14

PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels_glm14 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm14 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels_glm14)
write.csv(Sub_glm14, "Sub_glm14.csv", row.names=FALSE)
################################################################################

# Run model (glm15  best fit from optimum covs)
model_glm15 <- glm(Party ~ 1 + Q102089 + Q109244 + Q113181 + Q115611 + Q115899 +
                           Q116881 + Q121699 + Q98869, data = train, family = "binomial")
summary(model_glm15)
model_glm <- model_glm15

PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels_glm15 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm15 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels_glm15)
write.csv(Sub_glm15, "Sub_glm15.csv", row.names=FALSE)
