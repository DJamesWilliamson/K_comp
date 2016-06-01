
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
trainChiSq <- read.csv("train2016.csv", na.strings = "")
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

################################################################################
# Pairwise ChiSq between question variables
# ?expand.grid
mat <- expand.grid(names(trainChiSq)[8:ncol(trainChiSq)], 
                   names(trainChiSq)[8:ncol(trainChiSq)])
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

output_pairwise <- vector()
for(i in 1:nrow(mat)){
        resultsTable = table(trainChiSq[ , as.character(mat$Var1)[i]], 
                             trainChiSq[ , as.character(mat$Var2)[i]], 
                             useNA = "no")
        x <- chisq.test( resultsTable)
        output_pairwise[[i]] <- x$p.value < 0.05
        # output[[i]][2] <- x$p.value < 0.05
        # output <- cat(names(train)[i], x$p.value < 0.05, "\n"))
        # print(output)
}
output_pairwise

# head(mat)
# head(trainChiSq[ ,"Q124742"])
# head(trainChiSq[ , as.character(mat$Var1)[1]])
# head(mat, 8)


# testTable <- table(trainChiSq[, as.character(mat$Var1)[7]], 
#                    trainChiSq[ ,as.character(mat$Var2)[7]], 
#                    useNA = "no")
# testTable
# chisq.test(testTable)$p.value

mat$assocn <- output_pairwise
head(mat, 20)
# Find identities
mat$identity <- ifelse(mat$Var1 == mat$Var2, TRUE, FALSE)
# Find duplicates
mat$dup <- ifelse(paste0(mat$Var1, mat$Var2) == paste0(mat$Var2, mat$Var1), TRUE, FALSE)
nrow(mat[mat$identity == TRUE | mat$dup == TRUE, ])
mat[mat$Var1 == "Q96024" & mat$Var2 == "Q98197", ]
mat[mat$Var1 == "Q98197" & mat$Var2 == "Q96024", ]
head(paste0(mat$Var1, mat$Var2))
class(output_pairwise)
mat_pairwise <- matrix(output_pairwise, length(questions), length(questions))

as.numeric(mat_pairwise)
length(mat_pairwise)
dim(mat_pairwise)
rownames(mat_pairwise) <- names(trainChiSq[questions])
colnames(mat_pairwise) <- names(trainChiSq[questions])

# mat_pairwise[!lower.tri(mat_pairwise)] <- 0  # unnecessary if parameters set below
sum(mat_pairwise)

library(igraph)
?igraph
pairwise <- graph_from_adjacency_matrix(adjmatrix = mat_pairwise, 
                                        mode = "upper", 
                                        diag = FALSE,
                                        add.colnames = NULL, add.rownames = NA)
print(pairwise)
str(pairwise)
plot(pairwise, vertex.size=5, vertex.label=NA)

# How many vertices linked to more than x others
sum(degree(pairwise) > 20)
sum(degree(pairwise) > 50)
max(degree(pairwise))
min(degree(pairwise))

V(pairwise)$size = degree(pairwise)/2+2
plot(pairwise, vertex.label=NA)
