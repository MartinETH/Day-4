# Assignment 4 #
# Martin Gubler #

library(TraMineR)
data(biofam)
biofam.seq <- seqdef(biofam)


# 1 #
# Continuing with the biofam data, build a table with the sequence length, 
# the number of transitions, the number of subsequences, the longitudinal entropy, 
# the turbulence and the complexity index.

tab <- data.frame(seqlength(biofam.seq), seqtransn(biofam.seq), seqsubsn(biofam.seq), seqient(biofam.seq), seqST(biofam.seq), seqici(biofam.seq))
tab [1:5,]

# 2 #
# Using summary(), look at the min, max, mean, medians and quartiles of the distribution of each of the computed longitudinal characteristics.
summary(tab)

# 3 #
# Display the histogram of each longitudinal characteristic but the length in a same graphic.

par(mfrow(2,3))
hist(tab$Trans.)
hist(tab$Subseq.)
hist(tab$Entropy)
hist(tab$Turbulence)
hist(tab$C)

### QUESTION: I am not quite clear about the solution. 
### How exactly do you solve it with the "for..." command? 


# 4 #
# Generate the sequences of distinct successive states (DSS) and the table with the
# duration in the distinct successive states. Display the last 6 of them.

seqdss(biofam.seq) [1995:2000,]
  # or
tail(seqdss(biofam.seq))


# 5 #
# Compute the mean and the variance of the time spent in the successive states.
# Display their summaries. (Hint: use the apply function and specify na.rm=TRUE)

### QUESTION: Could not solve it myself, but I am not quite clear about the solution, either.
### What, exactly, does the "apply" function do? Could you please comment it briefly in the next session? Thank you.

# 6 #
# Generate a scatterplot matrix for comparing the Entropy with the Turbulence and Complexity Index.

plot(tab[,4:6])

# 7 #
# Compare the distributions of the complexity index by birth cohorts using boxplots.
par(mfrow = c(1,1))
boxplot(tab$C ~biofam$cohort)

# 8 #
# Regress the complexity index on the birth cohort, the sex and 
# the language of the questionnaire. Comment the results.

lm.complex <- lm(tab$C ~cohort + sex + plingu02, data=biofam)
summary(lm.complex)

## When controlling for other variables, complexity increases significantly...
## ...for cohorts born after 1930
## ...for women
## ...for those who did NOT fill in their survey in Italian.
