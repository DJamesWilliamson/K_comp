getwd()
list.files()

library(dplyr)
library(tidyr)

train <- read.csv("train2016.csv")
str(train)
summary(train)

length(unique(train$USER_ID))
str(train)
head(train$YOB, 100)
sum(is.na(train$YOB))
summary(train$YOB)

head(train$Gender, 100)
tail(train$Gender, 100)
sum(is.na(train$Gender))
summary(train$Gender)

head(train$Income, 100)
tail(train$Income, 100)
sum(is.na(train$Income))
summary(train$Income)

head(train$EducationLevel, 100)
tail(train$EducationLevel, 100)
sum(is.na(train$EducationLevel))
summary(train$EducationLevel)

head(train$Party, 100)
tail(train$Party, 100)
sum(is.na(train$Party))
summary(train$Party)

# Observations:
# All have unique USER_ID (int)
# YOB has 333 NAs and several improbable/impossible entries (int)
# Gender has 113 empty cells
# Income has 1028 empty cells
# HouseholdStatus has 450 empty cells
# EducationLevel has 866 empty cells 
# Party has complete data
# Each question has ~2000 empty cells