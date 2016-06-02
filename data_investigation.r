
#=---------------------------------------------------------
#
# Script by Paul Thomas - @maxthomais - June 2016
# Help automate data analysis for analytics edge kaggle competition
#
#=---------------------------------------------------------

train = read.csv("train2016.csv")

# helper to convert to 2 decimal places ...
asPrcnt <- function( arg1 ) {
  round( 100 * arg1, digits = 2 )
}


# check to see if the democrat v republican 
# split ratio for an answer is very biased!
checkForAnswerBias <- function( arg1Table, arg2Desc, arg3DiffMin, arg4DiffMax )
{
  # see how many possible answers ...
  numCols = ncol(arg1Table)

  # loop over all the answers ...
  for (i in 1:numCols)
  {
    # calc diff
    diff = abs(arg1Table[1,i] - arg1Table[2,i]) / (arg1Table[1,i] + arg1Table[2,i])
    
    # is there is bias to be seen!?
    if ((diff > arg3DiffMin) && (diff <= arg4DiffMax))
    {
      # calc splits
      dems = arg1Table[1,i] / (arg1Table[1,i] + arg1Table[2,i])
      reps = arg1Table[2,i] / (arg1Table[1,i] + arg1Table[2,i])

      #print out result
      cat(arg2Desc,i,"Dem% =", asPrcnt(dems), "Rep% =", asPrcnt(reps), "Diff% =", asPrcnt(dems-reps), "\n")
    }
  }
}


# e.g. mac v pc, seems that MAC users tend to more pro democrat ...
compTable = table(train$Party,train$Q110740)
compTable
checkForAnswerBias( compTable, "Q110740", 0.1, 1.0 )


# note we just want to check the Question columns!
questions = grep("^Q",names(train))

# loop over all the questions
for(i in questions)
{
  resultsTable = table(train$Party,train[[i]])
  #checkForAnswerBias( resultsTable, names(train)[i], 0.05, 0.10)   # 5 - 10 %
  checkForAnswerBias( resultsTable, names(train)[i], 0.10, 0.25)   # 10 - 25 %
  checkForAnswerBias( resultsTable, names(train)[i], 0.25, 1.00)   # > 25% diff
}

#################################################################################
# Import training set but replace blanks with NA
trainChiSq <- read.csv("train2016.csv", na.strings = c("", "NA"))
head(trainChiSq[ ,8:12], 15)
head(trainChiSq[ ,1:8], 15)
head(names(trainChiSq), 10)

# note we just want to check the Question columns!
questions = grep("^Q",names(trainChiSq))

?chisq.test

# Create vector identifying questions associated with outcome using Chi-squared.
# loop over all the questions
output_outcome <- vector()
for(i in questions){
        resultsTable = table(trainChiSq$Party,trainChiSq[[i]])
        x <- chisq.test( resultsTable)
        output_outcome[[i]] <- x$p.value < 0.05
        # output[[i]][2] <- x$p.value < 0.05
        # output <- cat(names(train)[i], x$p.value < 0.05, "\n"))
        # print(output)
}
output_outcome
sigQs <- names(trainChiSq)[output_outcome == TRUE]
sigQs <- sigQs[!is.na(sigQs)]
sigQs

################################################################################
# Pairwise ChiSq between question variables
# ?expand.grid
# names(trainChiSq)[questions]
# names(trainChiSq)[8:ncol(trainChiSq)]

mat <- expand.grid(names(trainChiSq)[questions], 
                   names(trainChiSq)[questions])
class(mat)
mat

# head(trainChiSq[ , 8:12], 100)
# as.character(mat[ ,1])
# names(train)[8:20]==as.character(mat$Var1)
# ?table
# 
# 
# testTable <- table(trainChiSq[, as.character(mat$Var1)[1]], 
#                    trainChiSq[ ,as.character(mat$Var2)[5]], 
#                    useNA = "no")
# testTable
# chisq.test(testTable)$p.value

output_pairwise_cut <- vector()
output_pairwise_pval <- vector()
for(i in 1:nrow(mat)){
        resultsTable = table(trainChiSq[ , as.character(mat$Var1)[i]], 
                             trainChiSq[ , as.character(mat$Var2)[i]], 
                             useNA = "no")
        x <- chisq.test( resultsTable)
        output_pairwise_cut[[i]] <- x$p.value < 0.000001
        output_pairwise_pval[[i]] <- x$p.value
}
head(output_pairwise_cut)
head(output_pairwise_pval)
hist(output_pairwise_pval, breaks = seq(from=0, to=1, by=0.01))

sum(output_pairwise_cut)/length(output_pairwise_cut)
sum(output_pairwise_pval < 0.05)/length(output_pairwise_pval)
sum(output_pairwise_pval < 0.000001)/length(output_pairwise_pval)




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
