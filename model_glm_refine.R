# From model_glm.R after setting up inTrain and inTest

# glm3
# Set up covariates to exclude from the analysis
covs <- names(train)
omit_covs <- c("USER_ID", "Party")

# Variables not associated with the outcome (Party) using ChiSq
insigQs <- read.csv("insigQs.csv", stringsAsFactors = FALSE)
insigQs <- insigQs$x

# Remove additional variables
omit_covs <- c(omit_covs, insigQs)
covs <- setdiff(covs, omit_covs)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

################################################################################
# Run model
model_glm3 <- glm(as.formula(model_formula), data = inTrain, family = "binomial")
summary(model_glm3)
model_glm <- model_glm3

# Make predictions on the test set:
PredTest = predict(model_glm, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# Accuracy
conf_matrix = table(inTest$Party, PredTestLabels)
# Function to check accuracy of a 2x2 confusion matrix
checkAccuracy2x2 = function(x) {
        accuracy = (x[1,1]+x[2,2])/(x[1,1]+x[1,2]+x[2,1]+x[2,2])
        print(accuracy)
}
checkAccuracy2x2(conf_matrix)

# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm3_back = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(backwards)$call
summary(backwards)$aic

# Refine model with step function (forwards)
nothing <- glm(Party ~ 1, data = inTest, family = "binomial")

forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm3)), 
                 direction = "forward")

PredTest = predict(forwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm3_forw = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

conf_matrix = table(inTest$Party, PredTestLabels_glm3_forw)
checkAccuracy2x2(conf_matrix)
summary(forwards)
summary(forwards)$call
summary(forwards)$aic

################################################################################
################################################################################
# glm4
# Set up covariates to include in the analysis

# Variables associated with the outcome (Party) using ChiSq
sigQs <- read.csv("sigQs.csv", stringsAsFactors = FALSE)
sigQs <- sigQs$x
covs <- c(nonq_covs, sigQs)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm4 <- glm(as.formula(model_formula), data = inTrain, family = "binomial")
summary(model_glm4)
model_glm <- model_glm4

# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm4_back = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(backwards)$call
summary(backwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm4_back)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (forwards)
nothing <- glm(Party ~ 1, data = inTest, family = "binomial")

forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm4)), 
                 direction = "forward")

PredTest = predict(forwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm4_forw = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(forwards)$call
summary(forwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm4_forw)
checkAccuracy2x2(conf_matrix)

################################################################################
# glm5

# Set up covariates to include in the analysis

# Variables associated with Questions associated with Party in ChiSq or with face validity
covs <- c(nonq_covs, q_pool_1)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm5 <- glm(as.formula(model_formula), data = inTrain, family = "binomial")
summary(model_glm5)
model_glm <- model_glm5

# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm5_back = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(backwards)$call
summary(backwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm5_back)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (forwards)
nothing <- glm(Party ~ 1, data = inTest, family = "binomial")

forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm5)), 
                 direction = "forward")

PredTest = predict(forwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm5_forw = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(forwards)$call
summary(forwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm5_forw)
checkAccuracy2x2(conf_matrix)

################################################################################
# glm6

# Set up covariates to include in the analysis

# As above but excluding those not associated with Party in ChiSq
covs <- c(nonq_covs, q_pool_2)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm6 <- glm(as.formula(model_formula), data = inTrain, family = "binomial")
summary(model_glm6)
model_glm <- model_glm6

# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm6_back = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(backwards)$call
summary(backwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm6_back)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (forwards)
nothing <- glm(Party ~ 1, data = inTest, family = "binomial")

forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm6)), 
                 direction = "forward")

PredTest = predict(forwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm6_forw = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(forwards)$call
summary(forwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm6_forw)
checkAccuracy2x2(conf_matrix)
################################################################################
# glm7

# Set up covariates to include in the analysis: use optimum covariates
covs <- c(cov_optimum)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm7 <- glm(as.formula(model_formula), data = inTrain, family = "binomial")
summary(model_glm7)
model_glm <- model_glm7

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm7 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(model_glm7)$call
summary(model_glm7)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm7)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm7_back = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(backwards)$call
summary(backwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm7_back)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (forwards)
nothing <- glm(Party ~ 1, data = inTest, family = "binomial")

forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm7)), 
                 direction = "forward")

PredTest = predict(forwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm7_forw = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(forwards)$call
summary(forwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm7_forw)
checkAccuracy2x2(conf_matrix)

################################################################################
# glm8

# Set up covariates to include in the analysis: use frequent covariates
covs <- c(cov_frequent)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm8 <- glm(as.formula(model_formula), data = inTrain, family = "binomial")
summary(model_glm8)
model_glm <- model_glm8

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm8 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(model_glm8)$call
summary(model_glm8)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm8)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm8_back = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(backwards)$call
summary(backwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm8_back)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (forwards)
nothing <- glm(Party ~ 1, data = inTest, family = "binomial")

forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm8)), 
                 direction = "forward")

PredTest = predict(forwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm8_forw = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(forwards)$call
summary(forwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm8_forw)
checkAccuracy2x2(conf_matrix)

################################################################################
# glm9: this seems to be the best when tested internally ~0.65 accuracy

# Set up covariates to include in the analysis: use consensus covariates
covs <- c(cov_consensus)

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_glm9 <- glm(as.formula(model_formula), data = inTrain, family = "binomial")
summary(model_glm9)
model_glm <- model_glm9

PredTest = predict(model_glm9, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm9 = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(model_glm9)$call
summary(model_glm9)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm9)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (backwards)
backwards <- step(model_glm)

PredTest = predict(backwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm9_back = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(backwards)$call
summary(backwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm9_back)
checkAccuracy2x2(conf_matrix)

# Refine model with step function (forwards)
nothing <- glm(Party ~ 1, data = inTest, family = "binomial")

forwards <- step(nothing, scope = list(lower = nothing, upper = formula(model_glm9)), 
                 direction = "forward")

PredTest = predict(forwards, newdata=inTest, type="response")
threshold = 0.5
PredTestLabels_glm9_forw = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

summary(forwards)$call
summary(forwards)$aic
conf_matrix = table(inTest$Party, PredTestLabels_glm9_forw)
checkAccuracy2x2(conf_matrix)






ls()







