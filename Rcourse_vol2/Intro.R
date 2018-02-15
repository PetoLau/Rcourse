## Introduction to R ----

# IDE - Rstudio

# installing and loading libraries (packages)
# install.packages("dplyr")

library(data.table)

# remove all variables from memory and garbage collection
rm(list=ls())
gc()

# help
?lm # linear regression
?glm # Generalized Linear Model - e.g. Logistic regression
?kmeans # centoid-based clustering
?hclust # hierarchical clustering

## basic data types in R ----
# data.frame ----
df <- data.frame(value = rnorm(10), id = sample(letters[1:3], 10, replace = T))
df
str(df)
summary(df)
class(df)
df[, "id"]
df[, 2]
df[df$id == "a",]

# average by id
sapply(levels(df$id), function(i) mean(df[df$id == i, "value"]))
# not very intuitive, bad readable...
# apply functions - loops

# data.table ----
dt <- data.table(value = rnorm(10), id = sample(letters[1:3], 10, replace = T))
dt
str(dt)
class(dt)

# average by id
dt[, mean(value), by = id]
# DT[i, j, by]
# i where
# j select
# by group by

# changing values by reference - memory efficient and fast
dt[, value := rnorm(10)] # `:=` operator for columns
dt
set(dt, i = 1L, j = "id", value = letters[4]) # set() for rows

# data.frame style
df$value <- rnorm(10)
df[1, "id"] <- letters[4]
df
# hups
# solution
levels(df$id) <- letters[1:4]
df[1, "id"] <- letters[4]
df

# matrix ----
mat <- matrix(1:9, nrow = 3, ncol = 3)

# multiplication
mat %*% mat
# addition
mat + mat

# subsetting
mat[1,]
mat[,1]
mat[1,1]

# list ----
# structure
list_1 <- list(var1 = rnorm(10), var2 = "R is best", var3 = letters[1:5], df = df)
str(list_1)
list_1$df$id
list_1[["df"]][["id"]]

# standard viz ----
plot(ts(df$value))
abline(h = mean(df$value), col = "red", lwd  = 2)

# quick build of own package
