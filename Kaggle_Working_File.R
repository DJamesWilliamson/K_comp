getwd()
# Kaggle competition working file
library(ProjectTemplate)
create.project('kaggle')
setwd("C:\\Users\\he49794\\GitHub\\K_comp\\kaggle\\data")

# Read in question list
library(xlsx)
question_list <- read.xlsx("Questions.xlsx", sheetIndex = 1, colIndex = 2:3)
# Index question list by corresponding column in train
question_list$idx <- match(question_list$Question_ID, names(trainChiSq))
head(question_list)
class(question_list)


# set up a test matrix to explore PCA using questions identified by ChiSq
head(trainChiSq)



# use combination of low and high correlation
lo_corr_Q <- which(degree(pairwise) < 10)
hi_corr_Q <- which(degree(pairwise) > 20)
names(lo_corr_Q)
df_PCA <- trainChiSq[ , c(names(lo_corr_Q), names(hi_corr_Q))]
head(df_PCA)

# questions with other than yes/no response       
exclude_qs <- c("Q122771", "Q120472", "Q120194", "Q118232", "Q117193", "Q116881", "Q115777", "Q115899", 
"Q114386", "Q113584", "Q111580", "Q110740", "Q101163", "Q98059", "Q119650", "Q106997",
"Q101162")
# colnames(df_PCA)
keep_cols <- which(!colnames(df_PCA) %in% exclude_qs)
# only keep yes/no responses
df_PCA <- df_PCA[ , keep_cols]

# str(df_PCA)
# df_PCA <- as.character(df_PCA)
# df_PCA[which(df_PCA == "Yes")] <- 1
# df_PCA[df_PCA == "No"] = 0
# warnings()

lapply(df_PCA, class)
lapply(df_PCA, levels)

# try imputation using this method in mice package:
# Check this:
# http://stats.stackexchange.com/questions/99185/simultaneous-imputation-of-multiple-binary-variables-in-r

library(mice)
df_imp <- mice(df_PCA, seed = 1233, method = "logreg") 
# logistic regression for binary variable 
#Generates multiple imputations for incomplete multivariate data by Gibbs sampling
str(df_imp)     # much more complicated structure, see suggestion
head(df_imp$data)
df_imp

df_complete <- complete(df_imp)
head(df_complete)

mat_complete <- data.matrix(df_complete) # convert to numeric matrix for correlation calculation 
head(mat_complete)
dim(mat_complete)
# change responses to 0/1
mat_complete[mat_complete == 1] <- 0
mat_complete[mat_complete == 2] <- 1
head(mat_complete)

# try PCA
library("pcaMethods")
# browseVignettes("pcaMethods")

md  <- prep(mat_complete, scale="none", center=FALSE)
dim(md)
resPCA <- pca(md, method="svd", center=FALSE, nPcs=5)
resSVDI <- pca(md, method="svdImpute", center=FALSE, nPcs=5)
# resBPCA <- pca(md, method="bpca", center=FALSE, nPcs=5)
# resPPCA <- pca(md, method="ppca", center=FALSE, nPcs=5)

q2SVDI <- Q2(resSVDI, md, fold=10)

slplot(resPCA)

plotPcs(resPPCA, pc=1:3, scoresLoadings=c(TRUE, FALSE))

plotPcs(resSVDI, pc=1:3, scoresLoadings=c(TRUE, FALSE))


















##############################################################################
##############################################################################
# remove any rows where all are NA
# idx_NA <- which(rowSums(is.na(md)) == ncol(md))
# length(idx_NA)
# md <- md[-idx_NA, ]

pc <- pca(md, nPcs=3, method="ppca", center=FALSE)

imputed <- completeObs(pc)














################################################################################
# change responses to 0/1
df_PCAf <- sapply(df_PCA, as.integer)
df_PCA[df_PCA == 1] <- 0
df_PCA[df_PCA == 2] <- 1
head(df_PCA)


mat_PCA <- as.matrix(temp_df)
dim(mat_PCA)
mat_PCA[1:10, 1:10]
class(mat_PCA)

# plot each variable against a range of others
par(mfrow = c(4, 4))
for(i in 1:16){
plot(jitter(mat_PCA[ ,30], 2), jitter(mat_PCA[ ,i], 2), cex = 0.1)
}
par(mfrow = c(1, 1))




# Methods for removing variables
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = train[ , !(names(train) %in% nonvars) ]
SongsTest = test[ , !(names(test) %in% nonvars) ])

model3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(model3)

covs = names(baseball)
remove6 = c("Team", "Playoffs", "RankPlayoffs", "G", "WorldSeries")
# then use
# ?setdiff
covs = setdiff(covs, remove6)
# OR
# idx = which(covs %in% remove6)
# covs = covs[-idx]

remove2 = c("RS", "W", "OBP", "SLG", "BA", "OOBP", "OSLG", "League")
covs = setdiff(covs, remove2)
covs
cor_covs = paste("baseball$",covs, sep = "")

vars.for.imputation = setdiff(names(loans), "not.fully.paid") #present in first but not second set












# Imputation and PCA: not working
?prcomp
prcomp(mat_PCA, na.rm = TRUE)

library(BiocInstaller)
biocLite("pcaMethods")

