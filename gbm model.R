getwd()
library(dismo)

# Import original data
train2016 <- read.csv("train2016.csv", na.strings = c("", "NA"))
test2016 <- read.csv("test2016.csv", na.strings = c("", "NA"))
str(train2016)
str(test2016)

# Import data
questions <- read.csv("questions.csv", stringsAsFactors = FALSE)
imp3_mode <- read.csv("imp3_mode.csv")
str(questions)
str(imp3_mode)

# Re-create the imputed train and test sets:
Party <- train2016$Party
train_imp3_mode <- cbind(imp3_mode[1:nrow(train2016), ], Party)
test_imp3_mode <- imp3_mode[(nrow(train2016)+1):nrow(imp3_mode), ]
names(train_imp3_mode)
names(test_imp3_mode)

names(train_imp3_mode) <- c(names(train2016)[-7], "Party")
names(test_imp3_mode) <- names(train2016)[-7]

train_imp3_mode$USER_ID <- train2016$USER_ID
test_imp3_mode$USER_ID <- test2016$USER_ID

str(train_imp3_mode)
str(test_imp3_mode)

# Rename imputed data
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

cov_sigQplus <- read.csv("sigQplus.txt", stringsAsFactors = FALSE, header = FALSE)
str(cov_sigQplus)
cov_sigQplus <- cov_sigQplus$V1

cov_all <- names(train)[-c(1, 108)]

# Model with gbm
lapply(train, class)
train$Party <- as.numeric(train$Party) - 1
mean(train$Party)       # 47% Republican
head(train$Party); head(train2016$Party)        # Republican  = 1
cov_sigQplus_idx <- which(names(train) %in% cov_sigQplus)
party_idx <- 108L

gbmModel_sigQplus <- gbm.step(data = train, gbm.x = cov_sigQplus_idx, gbm.y = party_idx,
                              n.folds = 10, n.trees = 50, max.trees = 10000)
class(gbmModel_sigQplus)
predictions <- predict(gbmModel_sigQplus, newdata = test, type = "response", n.trees = 50)

threshold = 0.5
PredTestLabels = as.factor(ifelse(predictions<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_gbm1 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
sum(Sub_gbm1$Predictions == "Republican")/nrow(Sub_gbm1)        # 15% Republican
write.csv(Sub_gbm1, "Sub_gbm1.csv", row.names=FALSE)

###############################################################################

cov_all_idx <- which(names(train) %in% cov_all)
party_idx <- 108L

gbmModel_all <- gbm.step(data = train, gbm.x = cov_all_idx, gbm.y = party_idx,
                              n.folds = 10, n.trees = 50, max.trees = 10000)

predictions <- predict(gbmModel_all, newdata = test, type = "response", n.trees = 50)

threshold = 0.5
PredTestLabels = as.factor(ifelse(predictions<threshold, "Democrat", "Republican"))

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_gbm2 = data.frame(USER_ID = test2016$USER_ID, Predictions = PredTestLabels)
sum(Sub_gbm2$Predictions == "Republican")/nrow(Sub_gbm2)        # 0% Republican
write.csv(Sub_gbm2, "Sub_gbm2.csv", row.names=FALSE)

################################################################################
# Try with gbm
library(gbm)

# Select covariates
covs <- cov_sigQplus

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

data <- data.frame(sapply(train, as.numeric))
class(data)
head(data)

# Run model
model_gbm3 <- gbm(as.formula(model_formula), data = data, distribution = "bernoulli",
                  n.trees = 400, interaction.depth = 3, shrinkage = 0.05,
                  bag.fraction = 0.5, keep.data = FALSE, cv.folds = 5)
nTrees <- gbm.perf(model_gbm3)
print(nTrees)   # 173
summary(model_gbm3)
#                         var   rel.inf
# Q109244                 Q109244 25.900155
# YOB                         YOB 11.570283
# Q113181                 Q113181  8.656627
# Q115611                 Q115611  8.416500
# HouseholdStatus HouseholdStatus  6.872142
# Q99480                   Q99480  3.236691
# Q101163                 Q101163  3.186183
# Q119851                 Q119851  3.180103
# Q102089                 Q102089  2.881232
# Q110740                 Q110740  2.700763
# Q116953                 Q116953  2.677634
# Q105840                 Q105840  2.588892
# Q115899                 Q115899  2.578114
# Q121699                 Q121699  2.564356
# Q120472                 Q120472  2.433575
# Q106272                 Q106272  2.233457
# Q116881                 Q116881  2.091301
# Q98869                   Q98869  2.004244
# Q118232                 Q118232  1.560108
# Q120379                 Q120379  1.528995
# Gender                   Gender  1.138647


# Make predictions on the test set:
PredTest = predict(model_gbm3, newdata=test, type="response", n.trees = nTrees)
# all entries 0.3493812!
#################################################################################

# Select covariates
covs <- cov_all

# Set up formulation
covs <- paste(covs, sep="", collapse=" + ") 
model_formula <- c("Party", covs) 
model_formula <- paste(model_formula, sep="", collapse=" ~ ")

# Run model
model_gbm4 <- gbm(as.formula(model_formula), data = data, distribution = "bernoulli",
                  n.trees = 400, interaction.depth = 3, shrinkage = 0.05,
                  bag.fraction = 0.5, keep.data = FALSE, cv.folds = 5)
nTrees <- gbm.perf(model_gbm4)
print(nTrees)  
summary(model_gbm4)
#                          var    rel.inf
# Q109244                 Q109244 18.0914787
# Q115611                 Q115611  5.6857106
# YOB                         YOB  3.5599705
# Q98197                   Q98197  3.4981425
# Income                   Income  3.4971843
# HouseholdStatus HouseholdStatus  2.7659929
# EducationLevel   EducationLevel  2.3551351
# Q113181                 Q113181  2.1783021
# Q99480                   Q99480  1.8224632
# Q119851                 Q119851  1.7064598
# Q101163                 Q101163  1.7025401
# Q107869                 Q107869  1.2924914
# Q110740                 Q110740  1.2223422
# Q98869                   Q98869  1.1530764
# Q104996                 Q104996  1.1388158
# Q115390                 Q115390  1.1189236
# Q120472                 Q120472  1.1101442
# Q100689                 Q100689  1.0742687
# Q108342                 Q108342  0.9301487
# Q116881                 Q116881  0.9186590
# Q115899                 Q115899  0.9129868
# Q102089                 Q102089  0.9107532
# Q106997                 Q106997  0.9045075
# Q98078                   Q98078  0.9019657
# Q120194                 Q120194  0.8688203

# Make predictions on the test set:
PredTest = predict(model_gbm4, newdata=test, type="response", n.trees = nTrees)
# all entries around 0.28!

################################################################################
# Trial of gbm using caret
library(caret)
library(pROC)

head(train)
head(test)


trainDummy <- dummyVars("~.",data=train, fullRank=F)
trainDF <- as.data.frame(predict(trainDummy,train))
print(names(trainDF))

testDummy <- dummyVars("~.",data=test, fullRank=F)
testDF <- as.data.frame(predict(testDummy,test))
print(names(testDF))



# what is the proportion of your outcome variable?
prop.table(table(trainDF$Party))

# save the outcome for the glmnet model
tempOutcome <- trainDF$Party  

# generalize outcome and predictor variables
outcomeName <- 'Party'
predictorsNames <- names(trainDF)[-c(1, 232)]

# get names of all caret supported models 
names(getModelInfo())

# pick model gbm and find out what type of model it is
getModelInfo()$gbm$type

trainDF$Party <- ifelse(trainDF$Party == 1,'Republican','Democrat')

# split data into training and testing chunks
set.seed(1234)
splitIndex <- createDataPartition(trainDF[,outcomeName], p = .75, list = FALSE, times = 1)
trainDF <- trainDF[ splitIndex,]
testDF  <- trainDF[-splitIndex,]

# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

# run model
objModel <- train(trainDF[,predictorsNames], as.factor(trainDF[,outcomeName]), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

# find out variable importance
summary(objModel)
#                                               rel.inf
# Q109244.Yes                                 25.4207113
# Q109244.No                                  13.9800760
# Q98197.No                                    8.3894820
# Q115611.Yes                                  6.1874875
# Q115611.No                                   2.8655192
# Q113181.Yes                                  2.1757419
# YOB^5                                        2.1376089
# Q99480.Yes                                   1.8904088
# Q115899.Circumstances                        1.8209148
# Q101163.Dad                                  1.8189007
# EducationLevel.Q                             1.7559452
# HouseholdStatus.Domestic Partners (no kids)  1.4837607
# Q100689.No                                   1.3786286
# Q102089.Rent                                 1.3130393
# Q98869.No                                    1.2899066
# Q101163.Mom                                  1.2681412
# Q113181.No                                   1.2599926
# Q115195.Yes                                  1.2089457
# EducationLevel^4                             1.1476641
# Q106042.Yes                                  1.0384262
# YOB.Q                                        1.0132002
# Q99480.No                                    0.9836050
# HouseholdStatus.Married (w/kids)             0.9421569
# Q118232.Idealist                             0.8413807
# Q100680.Yes                                  0.8241850
# Q103293.Yes                                  0.7866173

# find out model details
objModel
# Stochastic Gradient Boosting 
# 
# 4177 samples
# 230 predictor
# 2 classes: 'Democrat', 'Republican' 
# 
# Pre-processing: centered (230), scaled (230) 
# Resampling: Cross-Validated (3 fold) 
# Summary of sample sizes: 2785, 2785, 2784 
# Resampling results across tuning parameters:
#         
#         interaction.depth  n.trees  ROC        Sens       Spec     
# 1                   50      0.6710780  0.6504065  0.6067208
# 1                  100      0.6697622  0.6571816  0.5929757
# 1                  150      0.6686689  0.6495032  0.5929749
# 2                   50      0.6735785  0.6621500  0.5919594
# 2                  100      0.6681423  0.6526649  0.5807425
# 2                  150      0.6667107  0.6603433  0.5883745
# 3                   50      0.6682140  0.6526649  0.5985768
# 3                  100      0.6624572  0.6531165  0.5899121
# 3                  150      0.6582363  0.6549232  0.5761585
# 
# Tuning parameter 'shrinkage' was held constant at a value of 0.1
# Tuning parameter 'n.minobsinnode' was
# held constant at a value of 10
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 50, interaction.depth = 2, shrinkage = 0.1 and
# n.minobsinnode = 10.

# get predictions on your testing data

# class prediction
predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
head(predictions)
postResample(pred=predictions, obs=as.factor(testDF[,outcomeName]))

# probabilities 
predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')
head(predictions)
postResample(pred=predictions[[2]], obs=ifelse(testDF[,outcomeName]=='yes',1,0))
auc <- roc(ifelse(testDF[,outcomeName]=="Republican",1,0), predictions[[2]])
print(auc$auc)
# Area under the curve: 0.7112

################################################################################
# Full trial with train and test, no subsetting

# run model
objModel <- train(trainDF[,predictorsNames], as.factor(trainDF[,outcomeName]), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

# find out variable importance
summary(objModel)
#                                                                                  var    rel.inf
# Q109244.No                                                                   Q109244.No 34.9265464
# Q109244.Yes                                                                 Q109244.Yes 18.4533238
# Q115611.Yes                                                                 Q115611.Yes 13.5484484
# Q98197.Yes                                                                   Q98197.Yes  8.7327380
# Q98197.No                                                                     Q98197.No  3.2300602
# Q115611.No                                                                   Q115611.No  3.1383808
# Q101163.Dad                                                                 Q101163.Dad  2.5263931
# Q113181.Yes                                                                 Q113181.Yes  1.9064080
# HouseholdStatus.Domestic Partners (no kids) HouseholdStatus.Domestic Partners (no kids)  1.8562515
# Q113181.No                                                                   Q113181.No  1.6094126
# Q100689.Yes                                                                 Q100689.Yes  1.3492472
# Q119851.No                                                                   Q119851.No  0.9303035
# Q115390.No                                                                   Q115390.No  0.8704784
# Income.L                                                                       Income.L  0.8585603
# Q105840.Yes                                                                 Q105840.Yes  0.8055503
# Q99480.Yes                                                                   Q99480.Yes  0.7880441

# find out model details
objModel
# Stochastic Gradient Boosting 
# 
# 5568 samples
# 230 predictor
# 2 classes: 'Democrat', 'Republican' 
# 
# Pre-processing: centered (230), scaled (230) 
# Resampling: Cross-Validated (3 fold) 
# Summary of sample sizes: 3712, 3711, 3713 
# Resampling results across tuning parameters:
#         
#         interaction.depth  n.trees  ROC        Sens       Spec     
# 1                   50      0.6763592  0.6533385  0.6224748
# 1                  100      0.6731085  0.6472395  0.6144495
# 1                  150      0.6695454  0.6330046  0.6075731
# 2                   50      0.6738666  0.6492745  0.6159772
# 2                  100      0.6695929  0.6428330  0.5980156
# 2                  150      0.6675815  0.6418177  0.6018374
# 3                   50      0.6702637  0.6513008  0.6014595
# 3                  100      0.6649572  0.6475762  0.5884608
# 3                  150      0.6621625  0.6428316  0.5899868
# 
# Tuning parameter 'shrinkage' was held constant at a value of 0.1
# Tuning parameter 'n.minobsinnode' was held constant
# at a value of 10
# ROC was used to select the optimal model using  the largest value.
# The final values used for the model were n.trees = 50, interaction.depth = 1, shrinkage = 0.1 and n.minobsinnode = 10.

# get predictions on your testing data

# class prediction
predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
head(predictions)

#Prepare submission (note will need to incorporate USER_ID from original test2016)
Sub_gbm5 = data.frame(USER_ID = test2016$USER_ID, Predictions = predictions)
sum(Sub_gbm5$Predictions == "Republican")/nrow(Sub_gbm5) # 48% Republican
write.csv(Sub_gbm5, "Sub_gbm5.csv", row.names=FALSE)
