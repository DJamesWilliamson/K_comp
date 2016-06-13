# KAGGLE COMPETITION - GETTING STARTED
setwd("C:/Users/he49794/Work/Statistics Resources/edX/Analytics Edge/Kaggle")

# This script file is intended to help you get started on the Kaggle platform,
# and to show you how to make a submission to the competition.


# Let's start by reading the data into R Make sure you have downloaded these
# files from the Kaggle website, and have navigated to the directory where you
# saved the files on your computer

train = read.csv("train2016.csv")

test = read.csv("test2016.csv")

# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:

SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)

# And then make predictions on the test set:

PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs.
# You can make up to 5 submissions per day, so don't hesitate to just upload a
# solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the
# "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, PREDICTION = PredTestLabels)

write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle
# website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the
# competition, you will need to build better models!


################################################################################
# Missing data
# Let's look at how much data is missing
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train,2,pMiss) # columns
apply(train,1,pMiss) # rows
# % or rows with less than 5% missing data
sum(apply(train,1,pMiss) < 5)/nrow(train)
sum(apply(test,1,pMiss) < 5)/nrow(test)

library(mice)
md.pattern(train)
#########################################################
noAns <- training 

removeNA <- function(x){
        if(is.factor(x) & sum(is.na(x)) > 0){
                factor(as.character(x), levels = c(levels(x), "noResponse"))
        }else{
                x
        }
}

noAns <- as.data.frame(lapply(noAns, removeNA))
noAns[is.na(noAns)]<- "noResponse"
################################################################################
# Turning blanks into NA

df[df == ""] = NA
# This changes every column to character from factor so perhaps:
MyData<-as.data.frame( apply(MyData,2,as.factor))

# OR
train = read.csv("train2016.csv", na.strings = "")
test = read.csv("test2016.csv", na.strings = "")

################################################################################
#Imputation Trying to run mice on the full data is resulting in an error: too 
#many (1152) weights for me. I had the same error and I removed USER_ID, Party,
#and YOB and ran mice and it worked. I think YOB is the problem since it is a
#factor with 80 levels. 
################################################################################
library(dplyr)
train <- tbl_df(train)
View(train)
str(test)
