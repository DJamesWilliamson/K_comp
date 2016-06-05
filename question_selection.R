#################################################################################
# Import training set but replace blanks with NA
trainChiSq <- read.csv("train2016.csv", na.strings = c("", "NA"))
str(trainChiSq)
summary(trainChiSq)

# note we just want to check the Question columns!
questions = grep("^Q",names(trainChiSq))

# Create vector identifying questions associated with OUTCOME (Party) using various 
# parameters: chi squared (cut and value), accuracy (both diags), PPV (both responses)
# Loop over all the questions

party_pairwise_acc_diag1 <- vector()
party_pairwise_acc_diag2 <- vector()
party_pairwise_acc <- vector()
party_pairwise_ppv1 <- vector()
party_pairwise_ppv2 <- vector()
party_pairwise_ppv <- vector()
party_pairwise_cut_05 <- vector()
party_pairwise_cut_0005 <- vector()
party_pairwise_pval <- vector()
for(i in questions){
        resultsTable = table(trainChiSq$Party,trainChiSq[[i]])
        x <- chisq.test( resultsTable)
        party_pairwise_acc_diag1[i] <- (resultsTable[1,1]+resultsTable[2,2])/sum(resultsTable)
        party_pairwise_acc_diag2[i] <- (resultsTable[1,2]+resultsTable[2,1])/sum(resultsTable)
        party_pairwise_acc[i] <- party_pairwise_acc_diag1[i]/party_pairwise_acc_diag2[i]
        party_pairwise_ppv1[i] <- resultsTable[1,1]/(resultsTable[1,1]+resultsTable[2,1])
        party_pairwise_ppv2[i] <- resultsTable[1,2]/(resultsTable[1,2]+resultsTable[2,2])
        party_pairwise_ppv[i] <- party_pairwise_ppv1[i]/party_pairwise_ppv2[i]
        party_pairwise_cut_05[i] <- x$p.value < 0.05
        party_pairwise_cut_0005[i] <- x$p.value < 0.0005
        party_pairwise_pval[i] <- x$p.value
}

# Plot data to choose cut points for data
# hist(party_pairwise_pval, breaks = 100)
# length(party_pairwise_pval[party_pairwise_pval < 0.05])
# hist(-log(party_pairwise_pval), breaks = 100)
# 
# hist(party_pairwise_acc_diag1, breaks = 20)
# hist(party_pairwise_acc_diag2, breaks = 20)
# hist(party_pairwise_acc, breaks = 20)
# 
# hist(party_pairwise_ppv1, breaks = 20)
# hist(party_pairwise_ppv2, breaks = 20)
# hist(party_pairwise_ppv, breaks = 20)

# boxplot(party_pairwise_acc_diag2 ~ party_pairwise_cut)

# head(party_pairwise_acc_diag1, 20)
# head(party_pairwise_acc_diag2, 20)
# head(party_pairwise_acc, 20)
# head(party_pairwise_ppv, 20)
# head(party_pairwise_cut, 20)
# head(party_pairwise_pval, 20)

# Proportion of questions included at each p value (lowered for multiple comparison)
mean(party_pairwise_cut_05, na.rm = TRUE)
mean(party_pairwise_cut_0005, na.rm = TRUE)

# Review which type of selection to use: decided on ChiSq p value in the end
# seln_acc_diag1 <- which(party_pairwise_acc_diag1 < 0.44 | party_pairwise_acc_diag1 > 0.55)
# seln_acc_diag2 <- which(party_pairwise_acc_diag2 < 0.45 | party_pairwise_acc_diag2 > 0.56)
# seln_acc <- which(party_pairwise_acc < 0.7 | party_pairwise_acc > 1.3)
# 
# seln_ppv1 <- which(party_pairwise_ppv1 < 0.45 | party_pairwise_ppv1 > 0.6)
# seln_ppv2 <- which(party_pairwise_ppv2 < 0.45 | party_pairwise_ppv2 > 0.65)
# seln_ppv <- which(party_pairwise_ppv < 0.8 | party_pairwise_ppv > 1.2)

# This contains all of the others:
seln_cut_0005 <- which(party_pairwise_cut_0005)

sigQs <- names(trainChiSq)[seln_cut_0005]
sigQs <- sigQs[!is.na(sigQs)]
sigQs

# Note that Q113181 and Q98197 are almost identical, but Q113181 has fewer NAs
# plot(jitter(as.numeric(trainChiSq$"Q113181"), 2), jitter(as.numeric(trainChiSq$"Q98197"), 2))
# sum(is.na(trainChiSq$"Q113181"))
# sum(is.na(trainChiSq$"Q98197"))

sigQs <- sigQs[-which(sigQs == "Q98197")]
write.csv(sigQs, file = "sigQs.csv")

# Now select possible questions to exclude because unlikely associated with Party
seln_cut_05 <- which(party_pairwise_cut_05)

insigQs <- names(trainChiSq)[-seln_cut_05]
insigQs <- insigQs[grep("^Q",insigQs)]
insigQs
write.csv(insigQs, file = "insigQs.csv")
################################################################################
# Now repeat for higher threshold of cut (10 fold higher), ignoring other methods:

# party_pairwise_cut <- vector()
# party_pairwise_pval <- vector()
# for(i in questions){
#         resultsTable = table(trainChiSq$Party,trainChiSq[[i]])
#         x <- chisq.test( resultsTable)
#         party_pairwise_cut[i] <- x$p.value < 0.00001
#         party_pairwise_pval[i] <- x$p.value
# }
# seln_cut <- which(party_pairwise_cut)
# sigQs <- names(trainChiSq)[seln_cut]
# sigQs <- sigQs[!is.na(sigQs)]
# sigQs
# 
# # This adds another two questions
# 
# # Now repeat for higher threshold of cut (10 fold higher), ignoring other methods:
# party_pairwise_cut <- vector()
# party_pairwise_pval <- vector()
# for(i in questions){
#         resultsTable = table(trainChiSq$Party,trainChiSq[[i]])
#         x <- chisq.test( resultsTable)
#         party_pairwise_cut[i] <- x$p.value < 0.0001
#         party_pairwise_pval[i] <- x$p.value
# }
# seln_cut <- which(party_pairwise_cut)
# sigQs <- names(trainChiSq)[seln_cut]
# sigQs <- sigQs[!is.na(sigQs)]
# sigQs
# 
# # This adds another three questions
# # Final list of questions associated with Party in univariate (ChiSq) analysis
# sigQs <- sigQs[-which(sigQs == "Q98197")]

################################################################################
# Check each against the other for extreme correlation: doesn't appear to be any

mat_sigQs <- expand.grid(names(trainChiSq)[which(names(trainChiSq) %in% sigQs)], 
                   names(trainChiSq)[which(names(trainChiSq) %in% sigQs)])
class(mat_sigQs)
mat_sigQs

output_pairwise_cut <- vector()
output_pairwise_pval <- vector()
for(i in 1:nrow(mat_sigQs)){
        resultsTable = table(trainChiSq[ , as.character(mat_sigQs$Var1[i])], 
                             trainChiSq[ , as.character(mat_sigQs$Var2[i])], 
                             useNA = "no")
        x <- chisq.test( resultsTable)
        output_pairwise_cut[[i]] <- x$p.value < 0.0005
        output_pairwise_pval[[i]] <- x$p.value
}
head(output_pairwise_cut)
head(output_pairwise_pval)
idx_p.val <- order(output_pairwise_pval)
sigQPlots <- mat_sigQs[idx_p.val, ]

# Remove first one's which are matched against themselves
sigQPlots <- sigQPlots[(length(sigQs)+1):nrow(sigQPlots), ]
# Take every second plot (duplicated)
sigQPlots <- sigQPlots[seq(from = 1, to = nrow(sigQPlots), by = 2), ]

# Plot selected combinations
par(mfrow = c(2, 2))
for(i in 1:nrow(sigQPlots)){
        plot(jitter(as.numeric(trainChiSq[ , as.character(sigQPlots[i, 1])], 2)), 
             jitter(as.numeric(trainChiSq[ , as.character(sigQPlots[i, 2])], 2)),
             xlab = as.character(sigQPlots[i, 1]),
             ylab = as.character(sigQPlots[i, 2]),
             cex = 0.1)
}
par(mfrow = c(1, 1))
################################################################################
################################################################################
# Pairwise ChiSq between question variables (note more stringent p value)

# Create dataframe for pairwise comparisons
df_Qs2x2 <- expand.grid(names(trainChiSq)[questions], names(trainChiSq)[questions])
# Remove identities
df_Qs2x2 <- df_Qs2x2[-which(df_Qs2x2$Var1 == df_Qs2x2$Var2), ]

Qs2x2_pairwise_cut <- vector()
Qs2x2_pairwise_pval <- vector()
for(i in 1:nrow(df_Qs2x2)){
        resultsTable = table(trainChiSq[ , as.character(df_Qs2x2$Var1[i])], 
                             trainChiSq[ , as.character(df_Qs2x2$Var2[i])], 
                             useNA = "no")
        x <- chisq.test( resultsTable)
        Qs2x2_pairwise_cut[[i]] <- x$p.value < 0.000001
        Qs2x2_pairwise_pval[[i]] <- x$p.value
}

# Identify smallest p values
idx_p.val <- order(Qs2x2_pairwise_pval)
# Remove duplicates
idx_p.val <- idx_p.val[seq(from = 1, to = length(idx_p.val), by = 2)]

highCorPlots <- df_Qs2x2[idx_p.val, ]
# head(highCorPlots, 30)
# highCorPlots[highCorPlots$Var1 == "Q98197" & highCorPlots$Var2 == "Q116441", ]

# Plot highly correlated variables against a range of others
par(mfrow = c(3, 3))
for(i in 301:400){
        # for(j in 1:4){
                plot(jitter(as.numeric(trainChiSq[ , as.character(highCorPlots[i, 1])], 2)), 
                     jitter(as.numeric(trainChiSq[ , as.character(highCorPlots[i, 2])], 2)),
                     xlab = as.character(highCorPlots[i, 1]),
                     ylab = as.character(highCorPlots[i, 2]),
                     # main = j,
                     cex = 0.1)
        # }
}
par(mfrow = c(1, 1))

# Create list of pairwise correlated questions
corr_Qs <- list()
for(i in seq_along(names(trainChiSq)[questions])){
                # out <- list()
                corr_Qs[[i]] <- which(highCorPlots[ ,1] == names(trainChiSq)[questions[i]] |
                                highCorPlots[ ,2] == names(trainChiSq)[questions[i]])
                corr_Qs <- c(corr_Qs, corr_Qs[[i]])
}

# Reduce to 101 questions and add their names
corr_Qs <- corr_Qs[1:101]
names(corr_Qs) <- names(trainChiSq)[questions]
# Take top 15 correlations
corr_Qs <- lapply(corr_Qs, function(x) head(x, 15))

# Create list of actual questions
corr_Qs_list <- vector("list", 101)
for(i in 1:101)  {
        for(j in 1:15){
                s <- vector()
                s <- as.character(unlist(highCorPlots[corr_Qs[[i]][j], ]))
                corr_Qs_list[[i]][j] <- s[s != names(trainChiSq)[7 + i]]
                corr_Qs_list <- c(corr_Qs_list, corr_Qs_list[[i]][j])
        }
}

# Reduce to 101 questions and add their names
corr_Qs_list <- corr_Qs_list[1:101]
names(corr_Qs_list) <- names(trainChiSq)[questions]
corr_Qs_list

################################################################################
# Check that p values of last terms are reasonable (start with from = 15)
lastTerm <- unlist(corr_Qs_list)[seq(from = 7, to = length(unlist(corr_Qs_list)), by = 15)]
firstTerm <- names(corr_Qs_list)
check <- data.frame(firstTerm, lastTerm)
rownames(check) = NULL
colnames(check) = c("Var1", "Var2")

check_pairwise_pval <- vector()
for(i in 1:nrow(check)){
        resultsTable = table(trainChiSq[ , as.character(check$Var1[i])], 
                             trainChiSq[ , as.character(check$Var2[i])])
        x <- chisq.test( resultsTable)
        check_pairwise_pval[[i]] <- x$p.value
}
mean(check_pairwise_pval > 0.05)
which(check_pairwise_pval > 0.05)
# Shows that 37 and 94 only have first 6 comparisions with p > 0.05
# corr_Qs_list[37]
# corr_Qs_list[94]

# Could either set entries 7:15 to NULL or omit those items altogether
# Probably better to create the dataframe first
# corr_Qs_list_update <- corr_Qs_list
# corr_Qs_list_update[[37]] <- corr_Qs_list_update[[37]][-c(7:15)]
# corr_Qs_list_update[[94]] <- corr_Qs_list_update[[94]][-c(7:15)]
# corr_Qs_list_update

# Create dataframe of related questions for imputation
imp_Qs <- as.data.frame(corr_Qs_list)
cols <- 1:length(imp_Qs)
imp_Qs[ , cols] <- apply(imp_Qs[, cols], 2, function(x) as.character(x))
imp_Qs[7:15, c(37, 94)] <- NA

write.csv(imp_Qs, file = "imputation_questions.csv")

################################################################################
################################################################################


# test <- as.character(unlist(highCorPlots[corr_Qs[[1]][3], ]))
# names(trainChiSq)[7 + 1]
# corr_Qs[[1]]
# test <- test[test != names(trainChiSq)[7 + 1]]
# 
# test <- lapply(corr_Qs, length)
# head(unlist(test), 1000)
# hist(unlist(test))
# highCorPlots[ 436, ]
# highCorPlots[4536, ]
# highCorPlots[4604, ]
# highCorPlots[4757, ]
# 
# 
# names(trainChiSq)[questions]
# seq_along(questions)
# as.character(highCorPlots[1, 1])
# head(mat)
# head(trainChiSq[ ,"Q124742"])
# head(trainChiSq[ , as.character(mat$Var1)[1]])
# head(mat, 8)

# testTable <- table(trainChiSq[, as.character(mat$Var1)[7]], 
#                    trainChiSq[ ,as.character(mat$Var2)[7]], 
#                    useNA = "no")
# testTable
# chisq.test(testTable)$p.value

# mat$assocn <- output_pairwise_cut
# head(mat, 20)
# Find identities
# mat$identity <- ifelse(mat$Var1 == mat$Var2, TRUE, FALSE)
# Find duplicates
# mat$dup <- ifelse(paste0(mat$Var1, mat$Var2) == paste0(mat$Var2, mat$Var1), TRUE, FALSE)
# nrow(mat[mat$identity == TRUE | mat$dup == TRUE, ])
# mat[mat$Var1 == "Q96024" & mat$Var2 == "Q98197", ]
# mat[mat$Var1 == "Q98197" & mat$Var2 == "Q96024", ]
# head(paste0(mat$Var1, mat$Var2))
# class(output_pairwise_cut)


################################################################################
# create matrix with the pairwise correlated questions
# use df_Qs which includes identity matches (ie diagonals))
# re-run the ChiSq test using a variable p value (previously 0.000001 for sigQs)
df_Qs <- expand.grid(names(trainChiSq)[questions], names(trainChiSq)[questions])

Qgraph2x2_pairwise_cut_000001 <- vector()
Qgraph2x2_pairwise_cut_0001 <- vector()
Qgraph2x2_pairwise_cut_01 <- vector()
for(i in 1:nrow(df_Qs2x2)){
        resultsTable = table(trainChiSq[ , as.character(df_Qs$Var1[i])], 
                             trainChiSq[ , as.character(df_Qs$Var2[i])], 
                             useNA = "no")
        x <- chisq.test( resultsTable)
        Qgraph2x2_pairwise_cut_000001[[i]] <- x$p.value < 0.000001
        Qgraph2x2_pairwise_cut_0001[[i]] <- x$p.value < 0.0001
        Qgraph2x2_pairwise_cut_01[[i]] <- x$p.value < 0.01
}

cut_000001 <- Qgraph2x2_pairwise_cut_000001
cut_0001 <- Qgraph2x2_pairwise_cut_0001
cut_01 <- Qgraph2x2_pairwise_cut_01

mat_pairwiseQ <- matrix(cut_000001, length(questions), length(questions))

rownames(mat_pairwiseQ) <- names(trainChiSq[questions])
colnames(mat_pairwiseQ) <- names(trainChiSq[questions])

# mat_pairwise[!lower.tri(mat_pairwise)] <- 0  # unnecessary if parameters set below
# sum(mat_pairwise)


# create graph showing the relations (edges) between correlated questions (vertices)
# library(igraph)
# ?igraph
# take upper triangular matrix and exclude diagonal to avoid repeat/identity matches
pairwiseQ <- graph_from_adjacency_matrix(adjmatrix = mat_pairwiseQ, 
                                        mode = "upper", 
                                        diag = FALSE,
                                        add.colnames = NULL, add.rownames = NA)
# print(pairwiseQ)
# str(pairwiseQ)

# make size proportional to the degree of each vertex
par(mfrow = c(1, 1))
V(pairwiseQ)$size = degree(pairwiseQ)
plot(pairwiseQ, vertex.label=NA)

# How many vertices linked to more than x others
hist(degree(pairwiseQ))



no_corr_Q <- which(degree(pairwiseQ) == 0)
lo_corr_Q <- which(degree(pairwiseQ) < 10)
hi_corr_Q <- which(degree(pairwiseQ) > 20)

###############################################################################
# Read in question list
library(xlsx)
question_list <- read.xlsx("Questions.xlsx", sheetIndex = 1, colIndex = 2:3)
# Index question list by corresponding column in train
question_list$idx <- match(question_list$Question_ID, names(trainChiSq))

# High degree questions
Q_hi_corr <- hi_corr_Q[names(hi_corr_Q)]
question_list[question_list$Question_ID %in% names(Q_hi_corr), 1:2]
Q_hi_degree <- as.character(question_list[question_list$Question_ID %in% names(Q_hi_corr), 1])

write.csv(Q_hi_degree, file = "Q_hi_degree.csv")

# High degree questions also correlated with outcome (Party)
a <- hi_corr_Q[names(hi_corr_Q) %in% sigQs]
question_list[question_list$Question_ID %in% names(a), 1:2]

# Significant questions correlated with outcome (Party)
question_list[question_list$Question_ID %in% sigQs, 1:2]

# Insignificant questions unlikely correlated with outcome (Party)
question_list[question_list$Question_ID %in% insigQs, 1:2]




# Examine modified matrix with correlated questions for imputation
imp_q_matrix <- read.csv("imp_q_file.csv", header = TRUE)
head(imp_q_matrix)



################################################################################
################################################################################
