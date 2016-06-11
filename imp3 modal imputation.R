# Import original data
train2016 <- read.csv("train2016.csv", na.strings = c("", "NA"))
test2016 <- read.csv("test2016.csv", na.strings = c("", "NA"))
# str(train2016)
# str(test2016)

# Import sets from mice imputation of imp3 (using combination matrix?, 20 iter)
imp3_set1 <- read.csv("imp3_set1.csv")
names_for_imp <- names(imp3_set1)[-1]
imp3_set1 <- imp3_set1[ , -c(1)]
imp3_set1 <- as.matrix(imp3_set1)

imp3_set2<- read.csv("imp3_set2.csv")
imp3_set2 <- imp3_set2[ , -c(1)]
imp3_set2 <- as.matrix(imp3_set2)

imp3_set3<- read.csv("imp3_set3.csv")
imp3_set3 <- imp3_set3[ , -c(1)]
imp3_set3 <- as.matrix(imp3_set3)

imp3_set4<- read.csv("imp3_set4.csv")
imp3_set4 <- imp3_set4[ , -c(1)]
imp3_set4 <- as.matrix(imp3_set4)

imp3_set5<- read.csv("imp3_set5.csv")
imp3_set5 <- imp3_set5[ , -c(1)]
imp3_set5 <- as.matrix(imp3_set5)

# Create array of stacked sets
imp3 <- array(c(imp3_set1, imp3_set2, imp3_set3, imp3_set4, imp3_set5), dim = (c(6960, 106, 5)))

# Function to calculate mode
getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Get mode for each entry across all sets
imp3_mode <- apply(imp3, c(1,2), getmode)
imp3_mode
class(imp3_mode)
write.csv(imp3_mode, "imp3_mode.csv")
imp3_mode <-  as.data.frame((imp3_mode))
colnames(imp3_mode) <- names_for_imp
head(imp3_mode)

# Re-create the imputed train and test sets:
Party <- train2016$Party
train_imp3_mode <- cbind(imp3_mode[1:nrow(train2016), ], Party)
test_imp3_mode <- imp3_mode[(nrow(train2016)+1):nrow(imp3_mode), ]

str(train_imp3_mode)
str(test_imp3_mode)


################################################################################
# Same for imp1 which used predQuick
# Import sets from mice imputation of imp3 (using combination matrix, 5 iter)
imp1_set1 <- read.csv("imp1_set1.csv")
imp1_set1 <- imp1_set1[ , -c(1)]
imp1_set1 <- as.matrix(imp1_set1)

imp1_set2<- read.csv("imp1_set2.csv")
imp1_set2 <- imp1_set2[ , -c(1)]
imp1_set2 <- as.matrix(imp1_set2)

imp1_set3<- read.csv("imp1_set3.csv")
imp1_set3 <- imp1_set3[ , -c(1)]
imp1_set3 <- as.matrix(imp1_set3)

imp1_set4<- read.csv("imp1_set4.csv")
imp1_set4 <- imp1_set4[ , -c(1)]
imp1_set4 <- as.matrix(imp1_set4)

imp1_set5<- read.csv("imp1_set5.csv")
imp1_set5 <- imp1_set5[ , -c(1)]
imp1_set5 <- as.matrix(imp1_set5)

# Create array of stacked sets
imp1 <- array(c(imp1_set1, imp1_set2, imp1_set3, imp1_set4, imp1_set5), dim = (c(6960, 106, 5)))

# Get mode for each entry across all sets
imp1_mode <- apply(imp1, c(1,2), getmode)
imp1_mode
write.csv(imp1_mode, "imp1_mode.csv")

#################################################################################


