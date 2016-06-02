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

?prcomp
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

mat_PCA <- as.matrix(df_PCA)
dim(mat_PCA)
mat_PCA[1:10, 1:10]
head(mat_PCA[mat_PCA == "Yes"], 10)
head(mat_PCA[mat_PCA == "No"], 10)

?lapply

head(data.frame(as.character(df_PCA)))

prcomp(as.matrix(as.numeric(as.character(df_PCA))))


class(df_PCA[ , "Q98078"])

?levels
levels(df_PCA[ , "Q98078"])


df_PCA <- sapply(df_PCA[ , 1:46], function(x) levels(x) <- c(0, 1))
sapply(df_PCA[ , 1:46], levels)

?relevel.factor

ncol(df_PCA)
as.character(df_PCA)
df_PCA["Yes"] <- 1
df_PCA["No"] <- 0
df_mat <- as.matrix(as.logical(df_PCA))
df_mat


df[df == ""] = NA