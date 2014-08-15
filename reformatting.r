library(RUnit)
errMsg <- function(err) print(err)
load('reformatting-tests.rda')

# Implement the makeBinary function.
# args:
# <response.row>: a vector of integers giving the response values for each
#   question 
# <n.responses>: a vector of integers (same length as <response.row>)
#   indicating the number of possible responses for each question
#
# returns:
# a binary vector that reformats the responses of <response.row> as
# described in project1.pdf

makeBinary <- function(response.row, n.responses) {

  unlist(mapply(function(i, max) { temp = numeric(max); temp[i] = 1; temp }, response.row, n.responses))
}


tryCatch(checkEquals(make.binary.test1, makeBinary(make.binary.rr1,
                                                   make.binary.nr)),
         error=function(err) errMsg(err))

tryCatch(checkEquals(make.binary.test2, makeBinary(make.binary.rr2,
                                                   make.binary.nr)),
         error=function(err) errMsg(err))

# use your "makeBinary" function to reformat your "ling-data-clean.data"
# dataset. Store this as a dataframe with variable names and order **as
# indicated in project1.pdf**. Save this dataframe as the file
# "binary-ling-data.data".

filtered = read.table("ling-data-clean.data", header=T)
vars = grep("Q", colnames(filtered), value=T)
meta = setdiff(colnames(filtered), vars)
n.responses = apply(filtered[, vars], max, MARGIN=c(2))
binarified = t(apply(filtered[, vars], makeBinary, MARGIN=c(1), n.responses=n.responses))
binarified.names = unlist(mapply(function(name, num) sapply(1:num, function(i) paste(name, i, sep=".")), vars, n.responses))

final = cbind(filtered[, meta], binarified)
colnames(final) = c(meta, binarified.names)

write.table(final, file="binary-ling-data.data", row.names=F)
