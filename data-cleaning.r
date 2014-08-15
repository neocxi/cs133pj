# create a subset of the data in lingData.txt where all observations that
# omitted every question have been removed. Store the **number** of
# observations that you omitted as the variable <n.no.response>

raw = read.delim("lingData.txt", header=T, sep=" ")
vars = grep("Q", colnames(raw), value=T)
sums = apply(abs(raw[, vars]), sum, MARGIN=c(1))
zeros = sums == 0
# n.no.response <- your code here

n.no.response = sum(zeros)

filtered.data = raw[!zeros, ]

# plot a histogram of the number of omitted responses for each observation
# after removing observations that omitted all questions

blanks = apply(filtered.data[, vars] == 0, sum, MARGIN=c(1))
hist(blanks, main="Number Omitted Questions", xlab="questions omitted")

# using your subset (with observations that responded to no questions
# removed) find the 99th percentile cutoff for number of questions
# omitted. Remove all observations with more than this number of omitted
# questions.
cutoff = quantile(blanks, probs=c(0.99))
cleaned = filtered.data[blanks <= cutoff,]
write.table(cleaned, file="ling-data-clean.data", row.names=F)

# save the subset of remaining observations in a file named
# "ling-data-clean.data" 
