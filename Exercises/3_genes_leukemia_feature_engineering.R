# Genes Leukemia dataset - basic feature engineering ----
library(ggplot2)

datas <- read.csv("Exercises/genes-leukemia.csv", stringsAsFactors = F)
str(datas)
summary(datas)

# "?" transform to NA! It's a must.
datas[datas == "?"] <- NA
summary(datas)

# Categorical features exploration ----
table.freq <- table(datas[, c("CLASS", "Gender")])
table.freq[1,] <- table.freq[1,]/sum(table.freq[1,])
table.freq[2,] <- table.freq[2,]/sum(table.freq[2,])
DF1 <- as.data.frame(table.freq)

ggplot(DF1, aes(x = Gender, y = Freq, fill = CLASS)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_bw()

# Test for contingency tables of size 2x2 - Fisher test
# Null hypothesis - odds ratios are equal for categories - frequencies are equally divided in a table
fisher.test(table(datas[, c("CLASS", "Gender")]))

# Multiple classes in a category
table.freq <- table(datas[, c("CLASS", "Source")])
table.freq[1,] <- table.freq[1,]/sum(table.freq[1,])
table.freq[2,] <- table.freq[2,]/sum(table.freq[2,])
DF1 <- as.data.frame(table.freq)

ggplot(DF1, aes(x = Source, y = Freq, fill = CLASS)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_bw()

# Test for contingency tables of size MxN - Chi-square test
# Null hypothesis - odds ratios are equal for categories - frequencies are equally divided in a table
chisq.test(table(datas[, c("CLASS", "Source")]))

table.freq <- table(datas[, c("CLASS", "Year")])
# no data for AML
table.freq[1,] <- table.freq[1,]/sum(table.freq[1,])
table.freq[2,] <- table.freq[2,]/sum(table.freq[2,])
DF1 <- as.data.frame(table.freq)

ggplot(DF1, aes(x = Year, y = Freq, fill = CLASS)) +
 geom_bar(stat = "identity", width = 0.8) +
 theme_bw()

# Numerical features exploration ----
# subset gene expression data
int_cols <- sapply(datas, is.integer)
data_exp <- datas[, int_cols]

# PCA dimensionality reduction
# visualise data to 2D
data_pc <- prcomp(data_exp, center = TRUE, scale. = TRUE)

ggplot(data.frame(data_pc$x[,1:2], class = datas$CLASS), aes(PC1, PC2, color = class)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_bw()

# Normality testing
# before using t-test it is necessery to test normality of data, becasuse t-test assume normal distribution
shapiro.test(data_exp$D49950)
hist(data_exp$D49950, breaks = 20)
# diveded for class - better results
shapiro.test(data_exp$D49950[which(datas$CLASS == "ALL")])
shapiro.test(data_exp$D49950[which(datas$CLASS == "AML")])
hist(data_exp$D49950[which(datas$CLASS == "AML")], breaks = 15)

# Two sample mean testing
# Null hypothesis H_0: \mu_1 = \mu_2 vs. H_1: \mu_1 != \mu_2
t.test(data_exp$D49950[which(datas$CLASS == "ALL")],
       data_exp$D49950[which(datas$CLASS == "AML")], paired = F)

# Non-parametric version of t-test is Wilcoxon rank sum test (without parametric distibution assumption)
wilcox.test(data_exp$D49950[which(datas$CLASS == "ALL")],
            data_exp$D49950[which(datas$CLASS == "AML")],
            paired = F)
