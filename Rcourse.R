######## R COURSE #########

# INTRO TO SYNTAX #

x <- c(1, 4, 101, 6)
x
x[2]
x[3:4]

mean(x)
sd(x)
avg_x <- mean(x)
avg_x

?mean # help
?sd

x <- c(1, NA, 2) # Not Available, missing data
mean(x)
mean(x, na.rm = TRUE)
x <- na.omit(x) # remove (omit) NA values
mean(x)

x <- c(2:6)
x
x[-1] # remove first number
x[-c(1:2)]
x[5] <- 15
x

rep(5, 10) # repeat 5, 10 times 
seq(from=60, to=-30, length=3) # sequence
seq(from=12, to=30, by=3)

# Functions, Loops and Vectorization #

fun <- function(x, c){
  return((x+c)^2)
}

fun(1:3, 0)
fun(1:3, 1)
fun(1:3, 1:3)

x <- 2:4
y <- 3:5
N <- length(x)
new <- vector(length=N) # "alocation" in R
# not needed in R #
for(i in 1:N){
  new[i] <- x[i] + y[i]
}
new
# simple and fast #
x + y
sqrt(new)
x < new

# Filtering and Matrix #

x <- c(5,2,9,12,2,4,9)

x[x %% 2 == 0]
x[x == 2]
x[x == c(2,9)] # bad
x[x %in% c(2,9)] # good

ifelse(x > 6, 2*x, 3*x)

# more about c()
ch <- c("Rko", 5, 7)
mode(ch)
mode(x)

# Matrices

matrix(1:4)
matrix(1:4, nrow=2)
matrix(1:4, nrow=2, byrow = TRUE)
m <- matrix(1:9, ncol=3, byrow=TRUE)
m[1][2] # not like in C
m[1, 2]
m[, 1]
m - m
m * m # bad
m %*% m # mathematical matrix multiplication

cbind(m, 12:14) # add column to matrix
rbind(18:20, m) # add row to matrix
rbind(20:25, m) # must be same length

# apply function, no more loops in R

fun_norm <- function(x){
  return(x-mean(x)/sd(x)) 
} # normalization of vector

fun_norm(1:3)
scale(1:3)

apply(m, 1, fun_norm) # apply on rows
apply(m, 2, fun_norm) # apply on columns

# array - multidimensional matrix
arr <- array(c(m, 2*m), dim = c(3,3,2))
arr[,,2]
dim(arr)

# Data.frame, list #
m <- cbind(c("A", "B", "C"), m) # not good
df <- data.frame(ch=c("A", "B", "C"), m)
df[,1]
df <- data.frame(ch=c("A", "B", "C"), m, stringsAsFactors = FALSE)
df[,1]
str(df) # structure
summary(df) # main statistics

l_person <- list(Name="Jozef", Weight=80, Member_of_FIIT=TRUE) # structure like in C
l_person$Name
l_person[[1]]
l_person[["Name"]]

## EXAMPLES ##
rm(list=ls()) # remove all variables from memory
gc()
?gc

# get and set working directory
getwd()
setwd("C:\\Users\\Peterovic\\Downloads\\Dizertacka\\Rcourse\\")

ozone <- read.csv("Ozone.csv", header=TRUE, sep=";") # read data
# explore your data
str(ozone)
summary(ozone)
head(ozone)
tail(ozone)
dim(ozone)

# subset features
ozone <- ozone[, c("O3", "wind", "humidity", "temp", "dpg", "vis", "doy")]
# plot data
pairs(ozone, panel = panel.smooth)
# nicely...
pairs(ozone, panel = panel.smooth, pch=21, bg="blue", cex=0.8, lwd=2)

# Regression analysis #
linear.model <- lm(O3~., data=ozone)
attributes(linear.model)
summary(linear.model)
plot(linear.model)

lin.mod_2 <- lm(O3 ~ poly(wind, 2) + humidity + poly(temp, 2) + dpg + poly(doy, 2) + poly(vis,2), data=ozone)
summary(lin.mod_2)

lin.mod_3 <- lm(O3 ~ wind + humidity + poly(temp, 2) + poly(dpg, 2) + poly(doy, 2) + poly(vis,2), data=ozone)
summary(lin.mod_3)

# Robust linear model #
library(MASS) # read library (package)
?rlm

rlin.mod_1 <- rlm(O3 ~ wind + humidity + poly(temp, 2) + poly(dpg, 2) + poly(doy, 2) + poly(vis,2), data=ozone)
summary(rlin.mod_1)
attributes(rlin.mod_1)
rlin.mod_1$residuals


