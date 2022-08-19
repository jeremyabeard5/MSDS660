##### Week 7 Discussion - Joel Konitzer #####

library(data.table)
library(BSDA)
library(ggplot2)
library(car)

# Flu Shot ----------------------------------------------------------------

fs_dt <- fread('new_flu_shot.csv')
str(fs_dt)

#Remove NA
fs_dt <- fs_dt[complete.cases(fs_dt),]

#Covert to numeric
fs_dt$old.vaccine <- as.numeric(fs_dt$old.vaccine)
fs_dt$new.vaccine <- as.numeric(fs_dt$new.vaccine)

#Calculate median
median(fs_dt$old.vaccine)
median(fs_dt$new.vaccine)

# 1. Which non-parametric test did you use?
SIGN.test(x = fs_dt$old.vaccine, y = fs_dt$new.vaccine, alternative = 'greater')
# 2. Is there a significant difference in median flu antibodies detected with the new flu vaccine?
#   - p-value is greater than our level significance
#   - We fail to reject the null hypothesis and state that the medians are NOT significantly different
# 3. What is the p-value that supports your claim?
#   - p = 0.3953

# MPG ----------------------------------------------------------------

mpg_dt <- fread('mpg.tsv')
str(mpg_dt)

#Remove the observations of -999
mpg_dt <- mpg_dt[mpg_dt$V2 > 1,]

#Calculate median
median(mpg_dt$V1)
median(mpg_dt$V2)

# 1. Which non-parametric test did you use?
wilcox.test(mpg_dt$V1, mpg_dt$V2)
# 2. Is there a significant difference in median flu antibodies detected with the new flu vaccine?
#   - p-value is not low enough to be significant
#   - We  reject the null hypothesis and state that the medians ARE significantly different
# 3. What is the p-value that supports your claim?
#   - p = 2.2e-16

# Vocab ----------------------------------------------------------------
vocab_dt <- fread('vocab.csv')
str(vocab_dt)

#Calculate median
median(vocab_dt$before.training)
median(vocab_dt$after.training)

# 1. Which non-parametric test did you use?
# - I added alternative = less because my understanding is we would want to see vocab values increase after training
# - The first group must be less than the second group
SIGN.test(x = vocab_dt$before.training, y = vocab_dt$after.training, alternative = 'less')
# 2. Is there a significant difference in median flu antibodies detected with the new flu vaccine?
#   - p-value is not low enough to be significant
#   - We fail to reject the null hypothesis and state that the medians are NOT significantly different
# 3. What is the p-value that supports your claim?
#   - p = 0.0897

# Menu ----------------------------------------------------------------
menu_dt <- read.csv('menu.csv', row.names=NULL)
menu_dt <- menu_dt[, c('Item', 'Sodium')]
menu_dt$Item <- as.factor(menu_dt$Item)
menu_dt$Sodium <- as.numeric(menu_dt$Sodium)
str(menu_dt)

#View Sodium value distribution
hist(menu_dt$Sodium)      
menu_dt <- menu_dt[menu_dt$Sodium > 1,]
menu_dt
           
library(agricolae)
?kruskal
str(menu_dt)
krus <- kruskal(menu_dt$Sodium, trt = menu_dt$Category, console = T)
# Protein 15 is significantly different from 10 and 5.
plot(krus)

# 1. Which non-parametric test did you use?
#  - Kruskal was used since we need to find variation of sodium in food items.
SIGN.test(x = vocab_dt$before.training, y = vocab_dt$after.training, alternative = 'less')
# 2. Is there a significant difference in salt in the menu items? 
#   - There is a significant difference between menu items. Beverages appear to be the menu that is in a group of its own.
# 3. What is the p-value that supports your claim?
#   - The Kruskal test gave me a pvalue of 0, but I think this may be due to formatting? Hopefully someone can confirm this
