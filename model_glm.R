# glm models using imputed data (imp1&3_sets), and different variable sets
# Import data
questions <- read.csv("questions.csv", stringsAsFactors = FALSE)
train <- read.csv("train_imp1_set1.csv")
test <- read.csv("test_imp1_set1.csv")
str(questions)
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
# Identify basic covariates
q_covs <- questions$Question_ID
nonq_covs <- c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")
################################################################################
# Basic model to predict Party using all other variables in the dataset, 
# except for the first, X:
SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)
# This was used with imp3_set1 in TestSubmission and gave ~0.58

################################################################################
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

# Run model
model_glm3 <- glm(as.formula(model_formula), data = train, family = "binomial")
summary(model_glm3)
model_glm <- model_glm3

# Make predictions on the test set:
PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm3 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
write.csv(Sub_glm3, "Sub_glm3.csv", row.names=FALSE)
# Score 0.599 with imp3_set1 (glm1)
# Score 0.615 with imp1_set1 (glm3)
################################################################################
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
model_glm4 <- glm(as.formula(model_formula), data = train, family = "binomial")
summary(model_glm4)
model_glm <- model_glm4

# Make predictions on the test set:
PredTest = predict(model_glm, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_glm4 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
write.csv(Sub_glm4, "Sub_glm4.csv", row.names=FALSE)
# Score 0.597 with imp3_set1 (glm2)
# Score 0.619 with imp1_set1 (glm4)

################################################################################
# Explore variable options:
q_face_valid <- read.table("q_face_valid.txt", stringsAsFactors = FALSE)
str(q_face_valid)
q_face_valid <- q_face_valid$V1

# Questions associated with Party in ChiSq or with face validity ~55
q_pool_1 <- unique(c(sigQs, q_face_valid))
questions[questions$Question_ID %in% q_pool_1, 3]

# As above but excluding those not associated with Party in ChiSq ~29
q_pool_2 <- q_pool_1[-which(q_pool_1 %in% insigQs)]
questions[questions$Question_ID %in% q_pool_2, 3]


