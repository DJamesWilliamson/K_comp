# Useful code from Manuel Amunategui
# https://www.youtube.com/watch?v=qhvkVxuwvLk
# http://amunategui.github.io/

library(RCurl) # download https data
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.data'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
gisetteRaw <- read.table(textConnection(x), sep = '', header = FALSE, stringsAsFactors = FALSE)

urlfile <- "https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.labels"
x <- getURL(urlfile, ssl.verifypeer = FALSE)
g_labels <- read.table(textConnection(x), sep = '', header = FALSE, stringsAsFactors = FALSE)

print(dim(gisetteRaw))

## [1] 6000 5001



The gisetteRaw data frame has 5001 columns and that’s the kind of size we’re looking for. Before we can start the PCA transformation process, we need to remove the extreme near-zero variance as it won’t help us much and risks crashing the script. We load the caret package and call nearZeroVar function with saveMetrics parameter set to true. This will return a data frame with the degree of zero variance for each feature:
        
        library(caret)
nzv <- nearZeroVar(gisetteRaw, saveMetrics = TRUE)
print(paste('Range:',range(nzv$percentUnique)))

## [1] "Range: 0"   "Range: 8.6"

print(head(nzv))

##    freqRatio percentUnique zeroVar  nzv
## V1     48.25        5.2167   FALSE TRUE
## V2   1180.80        1.3667   FALSE TRUE
## V3     41.32        6.1500   FALSE TRUE
## V4   5991.00        0.1667   FALSE TRUE
## V5    980.00        1.5333   FALSE TRUE
## V6    140.00        3.5167   FALSE TRUE



We remove features with less than 0.1% variance:
        
        print(paste('Column count before cutoff:',ncol(gisetteRaw)))

## [1] "Column count before cutoff: 5001"

dim(nzv[nzv$percentUnique > 0.1,])

## [1] 4639    4

gisette_nzv <- gisetteRaw[c(rownames(nzv[nzv$percentUnique > 0.1,])) ]
print(paste('Column count after cutoff:',ncol(gisette_nzv)))

## [1] "Column count before cutoff: 4639"



The data is cleaned up and ready to go. Let’s see how well it performs without any PCA transformation. We bind the labels (response/outcome variables) to the set:
        
        dfEvaluate <- cbind(as.data.frame(sapply(gisette_nzv, as.numeric)),
                            cluster=g_labels$V1)



We’re going to feed the data into the following cross-validation function using the zxgboost model. This is a fast model and does great with large data sets. The repeated cross-validation will run the data 5 times, each time assigning a new chunk of data as training and testing. This not only allows us to use all the data as both train and test sets, but also stabilizes our AUC (Area Under the Curve) score.


EvaluateAUC <- function(dfEvaluate) {
        require(xgboost)
        require(Metrics)
        CVs <- 5
        cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
        indexCount <- 1
        outcomeName <- c('cluster')
        predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
        lsErr <- c()
        lsAUC <- c()
        for (cv in seq(1:CVs)) {
                print(paste('cv',cv))
                dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
                dataTest <- dfEvaluate[dataTestIndex,]
                dataTrain <- dfEvaluate[-dataTestIndex,]
                
                bst <- xgboost(data = as.matrix(dataTrain[,predictors]),
                               label = dataTrain[,outcomeName],
                               max.depth=6, eta = 1, verbose=0,
                               nround=5, nthread=4, 
                               objective = "reg:linear")
                
                predictions <- predict(bst, as.matrix(dataTest[,predictors]), outputmargin=TRUE)
                err <- rmse(dataTest[,outcomeName], predictions)
                auc <- auc(dataTest[,outcomeName],predictions)
                
                lsErr <- c(lsErr, err)
                lsAUC <- c(lsAUC, auc)
                gc()
        }
        print(paste('Mean Error:',mean(lsErr)))
        print(paste('Mean AUC:',mean(lsAUC)))
}

EvaluateAUC(dfEvaluate)

## [1] "cv 1"
## [1] "cv 2"
## [1] "cv 3"
## [1] "cv 4"
## [1] "cv 5"

## [1] 0.9659



This yields a great AUC score of 0.9659 (remember, AUC of 0.5 is random, and 1.0 is perfect). But we don’t really care how well the model did; we just want to use that AUC score as a basis of comparison against the transformed PCA variables.

So, let’s use the same data and run it through prcomp. This will transform all the related variables that account for most of the variation - meaning that the first component variable will be the most powerful variable (Warning: this can be a very slow to process depending on your machine - it took 20 minutes on my MacBook - so do it once and store the resulting data set for later use):
        
        pmatrix <- scale(gisette_nzv)
princ <- prcomp(pmatrix)



Let’s start by running the same cross-validation code with just the first PCA component (remember, this holds most of the variation of our data set). We need to use our princ result set and call the predict function to get our data.frame:
        
        nComp <- 1  
dfComponents <- predict(princ, newdata=pmatrix)[,1:nComp]

dfEvaluate <- cbind(as.data.frame(dfComponents),
                    cluster=g_labels$V1)

EvaluateAUC(dfEvaluate)

## [1] "cv 1"
## [1] "cv 2"
## [1] "cv 3"
## [1] "cv 4"
## [1] "cv 5"

## [1] 0.719



The resulting AUC of 0.719 isn’t that good compared to the orginal, non-transformed data set. But we have to remember that this is one variable against almost 5000!! Let’s try this again with 2 components:
        
        nComp <- 2  
...
print(mean(lsAUC))

## [1] 0.7228



Two components give an AUC score of 0.7228, still some ways to go. Let’s jump to 5 components:
        
        nComp <- 5
...
print(mean(lsAUC))

## [1] 0.9279



Now we’re talking, 0.9279!!! Let’s try 10 components:
        
        nComp <- 10
...
print(mean(lsAUC))

## [1] 0.9651



Yowza!! 0.9651!! Let’s try 20 components:
        
        nComp <- 20
...
print(mean(lsAUC))

## [1] 0.9641



Hmmm, going back down… Let’s stop here and stick with the first 10 PCA components. So, 10 PCA columns versus 4639 columns - not bad, right? Keep in mind that you should be able to get closer to the AUC of the original data set by adding more PCA components as prcomp accounts for all variations in the data. On the other hand, by following the steps in this walkthrough, you can get a great AUC score with very little effort and an absurdly smaller resulting data set.


Additional Stuff

A common critique about PCA is that it is hard to analyze once transformed as many of variables get clumped together under a nondescript name. One way around this is top plot your PCA data ontop of you discrete variables, see FactoMineR for more information.

Though out of scope for this hands-on post, there are many ways of finding the perfect amount of components to use - check out Eigen angles and vectors and check out also clusterboot.




Full source code (also on GitHub):
        
        
        require(ROCR)
require(caret)
require(ggplot2)

EvaluateAUC <- function(dfEvaluate) {
        require(xgboost)
        require(Metrics)
        CVs <- 5
        cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
        indexCount <- 1
        outcomeName <- c('cluster')
        predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
        lsErr <- c()
        lsAUC <- c()
        for (cv in seq(1:CVs)) {
                print(paste('cv',cv))
                dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
                dataTest <- dfEvaluate[dataTestIndex,]
                dataTrain <- dfEvaluate[-dataTestIndex,]
                
                bst <- xgboost(data = as.matrix(dataTrain[,predictors]),
                               label = dataTrain[,outcomeName],
                               max.depth=6, eta = 1, verbose=0,
                               nround=5, nthread=4, 
                               objective = "reg:linear")
                
                predictions <- predict(bst, as.matrix(dataTest[,predictors]), outputmargin=TRUE)
                err <- rmse(dataTest[,outcomeName], predictions)
                auc <- auc(dataTest[,outcomeName],predictions)
                
                lsErr <- c(lsErr, err)
                lsAUC <- c(lsAUC, auc)
                gc()
        }
        print(paste('Mean Error:',mean(lsErr)))
        print(paste('Mean AUC:',mean(lsAUC)))
}

##########################################################################################
## Download data
##########################################################################################

# https://archive.ics.uci.edu/ml/datasets/Gisette
# http://stat.ethz.ch/R-manual/R-devel/library/stats/html/princomp.html

# word of warning, this is 20mb - slow
library(RCurl) # download https data
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.data'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
gisetteRaw <- read.table(textConnection(x), sep = '', header = FALSE, stringsAsFactors = FALSE)

urlfile <- "https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.labels"
x <- getURL(urlfile, ssl.verifypeer = FALSE)
g_labels <- read.table(textConnection(x), sep = '', header = FALSE, stringsAsFactors = FALSE)

##########################################################################################
## Remove zero and close to zero variance
##########################################################################################

nzv <- nearZeroVar(gisetteRaw, saveMetrics = TRUE)
range(nzv$percentUnique)

# how many have no variation at all
print(length(nzv[nzv$zeroVar==T,]))

print(paste('Column count before cutoff:',ncol(gisetteRaw)))

# how many have less than 0.1 percent variance
dim(nzv[nzv$percentUnique > 0.1,])

# remove zero & near-zero variance from original data set
gisette_nzv <- gisetteRaw[c(rownames(nzv[nzv$percentUnique > 0.1,])) ]
print(paste('Column count after cutoff:',ncol(gisette_nzv)))

##########################################################################################
# Run model on original data set
##########################################################################################

dfEvaluate <- cbind(as.data.frame(sapply(gisette_nzv, as.numeric)),
                    cluster=g_labels$V1)

EvaluateAUC(dfEvaluate)

##########################################################################################
# Run prcomp on the data set
##########################################################################################

pmatrix <- scale(gisette_nzv)
princ <- prcomp(pmatrix)

# plot the first two components
ggplot(dfEvaluate, aes(x=PC1, y=PC2, colour=as.factor(g_labels$V1+1))) +
        geom_point(aes(shape=as.factor(g_labels$V1))) + scale_colour_hue()

# full - 0.965910574495451
nComp <- 5  
nComp <- 10  
nComp <- 90     
nComp <- 20  
nComp <- 50   
nComp <- 100   

# change nComp to try different numbers of component variables (10 works great)
nComp <- 10  # 0.9650
dfComponents <- predict(princ, newdata=pmatrix)[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents),
                    cluster=g_labels$V1)

EvaluateAUC(dfEvaluate)




GBM source code:
        
        
        require(ROCR)
require(caret)
require(ggplot2)

Evaluate_GBM_AUC <- function(dfEvaluate, CV=5, trees=3, depth=2, shrink=0.1) {
        require(caret)
        require(Metrics)
        CVs <- CV
        cvDivider <- floor(nrow(dfEvaluate) / (CVs+1))
        indexCount <- 1
        outcomeName <- c('cluster')
        predictors <- names(dfEvaluate)[!names(dfEvaluate) %in% outcomeName]
        lsErr <- c()
        lsAUC <- c()
        for (cv in seq(1:CVs)) {
                print(paste('cv',cv))
                
                dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
                dataTest <- dfEvaluate[dataTestIndex,]
                dataTrain <- dfEvaluate[-dataTestIndex,]
                
                
                dataTrain[,outcomeName] <- ifelse(dataTrain[,outcomeName]==1,'yes','nope')
                
                # create caret trainControl object to control the number of cross-validations performed
                objControl <- trainControl(method='cv', number=2, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
                
                # run model
                bst <- train(dataTrain[,predictors],  as.factor(dataTrain[,outcomeName]), 
                             method='gbm', 
                             trControl=objControl,
                             metric = "ROC",
                             tuneGrid = expand.grid(n.trees = trees, interaction.depth = depth, shrinkage = shrink)
                )
                
                predictions <- predict(object=bst, dataTest[,predictors], type='prob')
                auc <- auc(ifelse(dataTest[,outcomeName]==1,1,0),predictions[[2]])
                err <- rmse(ifelse(dataTest[,outcomeName]==1,1,0),predictions[[2]])
                
                lsErr <- c(lsErr, err)
                lsAUC <- c(lsAUC, auc)
                gc()
        }
        print(paste('Mean Error:',mean(lsErr)))
        print(paste('Mean AUC:',mean(lsAUC)))
}

# https://archive.ics.uci.edu/ml/datasets/Gisette
# http://stat.ethz.ch/R-manual/R-devel/library/stats/html/princomp.html

# word of warning, this is 20mb - slow
library(RCurl) # download https data
urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.data'
x <- getURL(urlfile, ssl.verifypeer = FALSE)
gisetteRaw <- read.table(textConnection(x), sep = '', header = FALSE, stringsAsFactors = FALSE)

urlfile <- "https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.labels"
x <- getURL(urlfile, ssl.verifypeer = FALSE)
g_labels <- read.table(textConnection(x), sep = '', header = FALSE, stringsAsFactors = FALSE)

# Remove zero and close to zero variance

nzv <- nearZeroVar(gisetteRaw, saveMetrics = TRUE)
range(nzv$percentUnique)

# how many have no variation at all
print(length(nzv[nzv$zeroVar==T,]))

print(paste('Column count before cutoff:',ncol(gisetteRaw)))

# how many have less than 0.1 percent variance
dim(nzv[nzv$percentUnique > 0.1,])

# remove zero & near-zero variance from original data set
gisette_nzv <- gisetteRaw[c(rownames(nzv[nzv$percentUnique > 0.1,])) ]
print(paste('Column count after cutoff:',ncol(gisette_nzv)))

# Run model on original data set

dfEvaluateOrig <- cbind(as.data.frame(sapply(gisette_nzv, as.numeric)),
                        cluster=g_labels$V1)

Evaluate_GBM_AUC(dfEvaluateOrig, CV=5, trees=10, depth=2, shrink=1) 

# Run prcomp on the data set

pmatrix <- scale(gisette_nzv)
princ <- prcomp(pmatrix)

# change nComp to try different numbers of component variables
nComp <- 20  
dfComponents <- predict(princ, newdata=pmatrix)[,1:nComp]
dfEvaluatePCA <- cbind(as.data.frame(dfComponents),
                       cluster=g_labels$V1)
Evaluate_GBM_AUC(dfEvaluatePCA,CV=5, trees=10, depth=2, shrink=1) 

