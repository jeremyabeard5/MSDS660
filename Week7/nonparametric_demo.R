##### NON-PARAMETRIC DEMO #####

library(data.table)
library(BSDA)

# Autism medication to reduce repetitive behaviors.
setwd("~/MSDS660_Instructor/Week7")
aut <- fread('autism_medication.csv')
View(aut)

#####################################################################
#Test if medians of the two groups differ significantly.

# Calculate median of the two groups
median(aut$Before.Treatment)
median(aut$After.Treatment)

# what we actually want to see is a reduction in the repetitive behaviors, so we should do a one-sided test
# The first group must be greater than the second group
?SIGN.test
SIGN.test(x = aut$Before.Treatment, y = aut$After.Treatment, alternative = 'greater')
# p-value = 0.1445 We fail to reject the null hypothesis and state that the medians are NOT significantly different.


# Wilcoxon Signed Rank test
wilcox.test(aut$Before.Treatment, aut$After.Treatment, alternative = 'greater')
# p-value = 0.06957 We fail to reject the null hypothesis and state that the medians are NOT significantly different.


# Mann-Whitney U test
?wilcox.test
wilcox.test(aut$Before.Treatment, aut$After.Treatment)
# p-value = 0.1391 not quite significant so we fail to reject the null hypnosis and 
# state that the medians are NOT significantly different


# one-sided -- We want the placebo to be greater than the drug
wilcox.test(aut$Before.Treatment, aut$After.Treatment, alternative = 'greater')
# p-value = 0.06957 are significant so we reject the null hypnosis and 
# state that the medians ARE significantly different.

# Kruskal-Wallis
# Amount of albumin in the blood with different amount of protein in diet
# Albumin can be used to gauge if enough protein is in the diet
alb <- fread('albumin.csv')
View(alb)
str(alb)
alb[, pct.protein.5:=as.numeric(pct.protein.5)]
str(alb)

kruskal.test(alb)
# Reject the null because p < 0.05
# At least one some of the groups has different medians


# complicated reshaping to work with agricolae
alb$temp <- 0 # temp column to allow reshape
# convert column names into row values
alb <- melt(alb, id.vars = 4, variable.name = 'protein', value.name = 'albumin')
alb <- alb[complete.cases(alb), ] # drop NAs
alb$temp <- NULL  # drop temp column that allowed for reshape
alb
str(alb)
library(agricolae)
krus <- kruskal(alb$albumin, trt = alb$protein, console = T)
# Protein 15 is significantly different from 10 and 5.
plot(krus)

################################################################
# Test is distributions differ

# Kolmogorov-Smirnov
ks.test(aut$Before.Treatment, aut$After.Treatment)
# confusingly, 'greater' for ks.test means the second set of data's median is greater than the first one
# so if we want the new drug less than the placebo, it's written like this:

ks.test(aut$After.Treatment, aut$Before.Treatment, alternative = 'greater')
# We fail to reject the null hypothesis and state that the distributions are NOT significantly different




#Anderson-Darling test
library(nortest)
ad.test(aut$Before.Treatment)

###############################################################################
# Test if variables are correlated

# spearman's
cor(aut[, c('Before.Treatment', 'After.Treatment'), with=F], method = 'spearman')
library(pspearman)
# rho is the spearman coefficient.  the p-value is used to check if there is a strong 
# monotonic relationship between the two variables
spearman.test(aut$Before.Treatment, aut$After.Treatment)
# We fail to reject the null hypothesis and there is NO relationship between variables.

#Kendall's Tau 
cor(aut[, c('Before.Treatment', 'After.Treatment'), with=F], method = 'kendall')
kendall(aut$Before.Treatment, aut$After.Treatment)
# We failt to reject the null hypothesis and there is NO relationship between variables.