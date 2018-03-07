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

fisher.test(table(datas[, c("CLASS", "Gender")]))

table.freq <- table(datas[, c("CLASS", "Source")])
table.freq[1,] <- table.freq[1,]/sum(table.freq[1,])
table.freq[2,] <- table.freq[2,]/sum(table.freq[2,])
DF1 <- as.data.frame(table.freq)

ggplot(DF1, aes(x = Source, y = Freq, fill = CLASS)) +
  geom_bar(stat = "identity", width = 0.8) +
  theme_bw()

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
int_cols <- sapply(datas, is.integer)
data_exp <- datas[, int_cols]

# PCA
data_pc <- prcomp(data_exp, center = TRUE, scale. = TRUE)

ggplot(data.frame(data_pc$x[,1:2], class = datas$CLASS), aes(PC1, PC2, color = class)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_bw()

# Normality testing
shapiro.test(data_exp$D49950)
hist(data_exp$D49950, breaks = 20)
shapiro.test(data_exp$D49950[which(datas$CLASS == "ALL")])
shapiro.test(data_exp$D49950[which(datas$CLASS == "AML")])
hist(data_exp$D49950[which(datas$CLASS == "AML")], breaks = 15)

# Two sample mean testing
t.test(data_exp$D49950[which(datas$CLASS == "ALL")], data_exp$D49950[which(datas$CLASS == "AML")], paired = F)
wilcox.test(data_exp$D49950[which(datas$CLASS == "ALL")], data_exp$D49950[which(datas$CLASS == "AML")], paired = F)
