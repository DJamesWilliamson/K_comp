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
party_pairwise_cut <- vector()
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
        party_pairwise_cut[i] <- x$p.value < 0.000001
        party_pairwise_pval[i] <- x$p.value
}

# Plot data to choose cut points for data
hist(party_pairwise_pval, breaks = 100)

hist(party_pairwise_acc_diag1, breaks = 20)
hist(party_pairwise_acc_diag2, breaks = 20)
hist(party_pairwise_acc, breaks = 20)

hist(party_pairwise_ppv1, breaks = 20)
hist(party_pairwise_ppv2, breaks = 20)
hist(party_pairwise_ppv, breaks = 20)

# boxplot(party_pairwise_acc_diag2 ~ party_pairwise_cut)

# head(party_pairwise_acc_diag1, 20)
# head(party_pairwise_acc_diag2, 20)
# head(party_pairwise_acc, 20)
# head(party_pairwise_ppv, 20)
# head(party_pairwise_cut, 20)
# head(party_pairwise_pval, 20)


# Review which type of selection to use: decided on ChiSq p value in the end
seln_acc_diag1 <- which(party_pairwise_acc_diag1 < 0.44 | party_pairwise_acc_diag1 > 0.55)
seln_acc_diag2 <- which(party_pairwise_acc_diag2 < 0.45 | party_pairwise_acc_diag2 > 0.56)
seln_acc <- which(party_pairwise_acc < 0.7 | party_pairwise_acc > 1.3)

seln_ppv1 <- which(party_pairwise_ppv1 < 0.45 | party_pairwise_ppv1 > 0.6)
seln_ppv2 <- which(party_pairwise_ppv2 < 0.45 | party_pairwise_ppv2 > 0.65)
seln_ppv <- which(party_pairwise_ppv < 0.8 | party_pairwise_ppv > 1.2)

# This contains all of the others:
seln_cut <- which(party_pairwise_cut)

sigQs <- names(trainChiSq)[seln_cut]
sigQs <- sigQs[!is.na(sigQs)]
sigQs

# Note that Q113181 and Q98197 are almost identical, but Q113181 has fewer NAs
plot(jitter(as.numeric(trainChiSq$"Q113181"), 2), jitter(as.numeric(trainChiSq$"Q98197"), 2))
sum(is.na(trainChiSq$"Q113181"))
sum(is.na(trainChiSq$"Q98197"))

sigQs <- sigQs[-which(sigQs == "Q98197")]


################################################################################
# Now repeat for higher threshold of cut (10 fold higher), ignoring other methods:

party_pairwise_cut <- vector()
party_pairwise_pval <- vector()
for(i in questions){
        resultsTable = table(trainChiSq$Party,trainChiSq[[i]])
        x <- chisq.test( resultsTable)
        party_pairwise_cut[i] <- x$p.value < 0.00001
        party_pairwise_pval[i] <- x$p.value
}
seln_cut <- which(party_pairwise_cut)
sigQs <- names(trainChiSq)[seln_cut]
sigQs <- sigQs[!is.na(sigQs)]
sigQs

# This adds another two questions

# Now repeat for higher threshold of cut (10 fold higher), ignoring other methods:
party_pairwise_cut <- vector()
party_pairwise_pval <- vector()
for(i in questions){
        resultsTable = table(trainChiSq$Party,trainChiSq[[i]])
        x <- chisq.test( resultsTable)
        party_pairwise_cut[i] <- x$p.value < 0.0001
        party_pairwise_pval[i] <- x$p.value
}
seln_cut <- which(party_pairwise_cut)
sigQs <- names(trainChiSq)[seln_cut]
sigQs <- sigQs[!is.na(sigQs)]
sigQs

# This adds another three questions
# Final list of questions associated with Party in univariate (ChiSq) analysis
sigQs <- sigQs[-which(sigQs == "Q98197")]

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
        output_pairwise_cut[[i]] <- x$p.value < 0.000001
        output_pairwise_pval[[i]] <- x$p.value
}
head(output_pairwise_cut)
head(output_pairwise_pval)
idx_p.val <- order(output_pairwise_pval)
sigQPlots <- mat_sigQs[idx_p.val, ]
# Remove first 12
sigQPlots <- sigQPlots[13:144, ]
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
# Pairwise ChiSq between question variables

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
################################################################################


test <- as.character(unlist(highCorPlots[corr_Qs[[1]][3], ]))
names(trainChiSq)[7 + 1]
corr_Qs[[1]]
test <- test[test != names(trainChiSq)[7 + 1]]

test <- lapply(corr_Qs, length)
head(unlist(test), 1000)
hist(unlist(test))
highCorPlots[ 436, ]
highCorPlots[4536, ]
highCorPlots[4604, ]
highCorPlots[4757, ]


names(trainChiSq)[questions]
seq_along(questions)
as.character(highCorPlots[1, 1])
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

# create matrix with the pairwise correlated questions
mat_pairwise <- matrix(output_pairwise_cut, length(questions), length(questions))

# as.numeric(mat_pairwise)
# length(mat_pairwise)
# dim(mat_pairwise)
rownames(mat_pairwise) <- names(trainChiSq[questions])
colnames(mat_pairwise) <- names(trainChiSq[questions])

# mat_pairwise[!lower.tri(mat_pairwise)] <- 0  # unnecessary if parameters set below
# sum(mat_pairwise)


# create graph showing the correlations (edges) between questions (vertices)
library(igraph)
# ?igraph
# take upper triangular matrix and exclude diagonal to avoid repeat/identity matches
pairwise <- graph_from_adjacency_matrix(adjmatrix = mat_pairwise, 
                                        mode = "upper", 
                                        diag = FALSE,
                                        add.colnames = NULL, add.rownames = NA)
# print(pairwise)
# str(pairwise)

# make size proportional to the degree of each vertex
V(pairwise)$size = degree(pairwise)/2+2
plot(pairwise, vertex.label=NA)

# How many vertices linked to more than x others
# sum(degree(pairwise) > 20)
# sum(degree(pairwise) > 50)
# max(degree(pairwise))
# min(degree(pairwise))


no_corr_Q <- which(degree(pairwise) == 0)
lo_corr_Q <- which(degree(pairwise) < 10)
hi_corr_Q <- which(degree(pairwise) > 20)

# Ensure no overlap between groups
# sort(names(no_corr_Q))
# sort(names(lo_corr_Q))
# sort(names(hi_corr_Q))
# names(no_corr_Q) %in% names(lo_corr_Q)
# names(lo_corr_Q) %in% names(hi_corr_Q)        


# names(no_corr_Q) %in% sigQs
# names(lo_corr_Q) %in% sigQs
# names(hi_corr_Q) %in% sigQs

# High degree questions also correlated with outcome
a <- hi_corr_Q[names(hi_corr_Q) %in% sigQs]
question_list[question_list$Question_ID %in% names(a), 1:2]

# High degree questions not correlated with outcome
b <- hi_corr_Q[!c(names(hi_corr_Q) %in% sigQs)]
question_list[question_list$Question_ID %in% names(b), 1:2]

# Low degree questions also correlated with outcome
c <- lo_corr_Q[c(names(lo_corr_Q) %in% sigQs)]
question_list[question_list$Question_ID %in% names(c), 1:2]

# Low degree questions not correlated with outcome
d <- lo_corr_Q[!c(names(lo_corr_Q) %in% sigQs)]
question_list[question_list$Question_ID %in% names(d), 1:2]

