######## R COURSE #########

## INTRO TO SYNTAX ##

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

# Matrices #

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
  return((x-mean(x))/sd(x)) 
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
cbind(c("A", "B", "C"), m) # not good
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

# O3 - Daily maximum one-hour-average ozone reading
# wind - Wind speed in LA airport
# humidity - Humidity at LA
# temp - Temperature 
# dpg - Pressure gradient (mm Hg)
# vis - Visibility (miles) measured at LA
# doy - Day of Year

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
# y = Beta_0 + Beta_1*x_1 + Beta_2*x_2 + ...
linear.model <- lm(O3~., data=ozone)

attributes(linear.model)
linear.model$terms
summary(linear.model)
plot(linear.model)

# nonlinear dependence #
lin.mod_2 <- lm(O3 ~ poly(wind, 2) + humidity + poly(temp, 2) + dpg + poly(doy, 2) + poly(vis,2), data=ozone)
summary(lin.mod_2)

# wind only linear, dpg -> poly #
lin.mod_3 <- lm(O3 ~ wind + humidity + poly(temp, 2) + poly(dpg, 2) + poly(doy, 2) + poly(vis,2), data=ozone)
summary(lin.mod_3)

# Robust linear model #
library(MASS) # read library (package)
?rlm

rlin.mod_1 <- rlm(O3 ~ wind + humidity + poly(temp, 2) + poly(dpg, 2) + poly(doy, 2) + poly(vis,2), data=ozone)
summary(rlin.mod_1)
attributes(rlin.mod_1)
rlin.mod_1$fitted.values

# GRAPHICS - Creating Graphs #

plot(ozone$temp, ozone$O3) # boring

plot(ozone$temp, ozone$O3, xlab = "Temperature", ylab = "Ozone", main = "Regression analysis") # Labels

plot(ozone$temp, ozone$O3, pch = 21, bg = "dodgerblue2", col = "darkorange", cex = 1.5,
     xlab = "Temperature", ylab = "Ozone", main = "Regression analysis") # nice

# Regression lines
lm_o3_temp <- lm(O3 ~ temp, data= ozone)
abline(lm_o3_temp, lwd = 2, col = "firebrick2")

lm_o3_temp2 <- lm(O3 ~ poly(temp, 2), data = ozone)
points(ozone$temp, as.vector(lm_o3_temp2$fitted.values), pch = 23, bg = "springgreen3")

# Plot legend of model without border
lm_o3_temp2$coefficients

legend("topleft", legend = c(paste("Model"),
                             paste("y =", round(lm_o3_temp2$coefficients[1], digits=3), " + ",
                             round(lm_o3_temp2$coefficients[2], digits=3), "x + ",
                             round(lm_o3_temp2$coefficients[3], digits=3), "x^2")),
                             bty = "n", text.font = 7)

ch <- "Hello!"
paste("Hello World!", ch)
round(2.23232323, digits = 3)
###################################################################################################

## TIME SERIES ##

Load <- read.csv("Load.csv", sep=";", header = TRUE)

str(Load)
summary(Load)
head(Load)
tail(Load)
dim(Load)

# set date format
Load[,5] <- as.Date(Load[,5]) #, "%d.%m.%Y %H:%M:%S"
str(Load)

## Find out number of OOM and range of days
ooms <- as.factor(Load[,4]) # something like character
levels(ooms)
datum <- as.factor(Load[,5])
levels(datum)

# install package 'forecast'
install.packages("forecast")
library(forecast)

# subset 1 OOM
ts_oom1 <- ts(Load[Load[,"oom_id"] %in% as.integer(levels(ooms)[2]) , "MNOZSTVO"], freq=96, start=0)
plot(ts_oom1)

# subset 8 days in dataset
length(levels(datum))

ts_oom1_week <- ts(ts_oom1[((79*96)+1):(87*96)], freq=96, start=79)
plot(ts_oom1_week)

# create train (7 days) and test (1 day) set
train <- ts(ts_oom1_week[1:(7*96)], freq=96, start=79)
test <- ts(ts_oom1_week[((7*96)+1):(8*96)], freq=96, start=86)

# Train HW model
HW <- HoltWinters(train, beta= FALSE)
HW.f <- forecast(HW, 96) # predict next 96 (1 day) values
attributes(HW.f)
HW.f$mean # prediction

# function to prediction error - MAPE - Mean Absolute Percentage Error
mape <- function(actual, pred){
  100*mean(abs((actual-pred)/actual))
}

mape(test, HW.f$mean)
accuracy(HW.f$mean, test) # function in forecast package

# Plot results
plot(ts_oom1_week, type = "n",  xlab = "Time (days)", ylab = "Load (kWh)", main = "Real values vs. Prediction")
lines(train)
lines(test, lwd = 2, col = "blue")
lines(HW.f$mean, lwd = 2, col = "red")

# STL decomposition + ARIMA model
stl.dekom <- stl(train, s.window="periodic", robust = TRUE)
plot(stl.dekom)

stl.arima.f <- forecast(stl.dekom, h = 96, method = "arima")
mape(test, stl.arima.f$mean)
lines(stl.arima.f$mean, lwd = 2, col = "green")
###################################################################################################################

## Cluster analysis ##
rm(list=ls()) # remove all variables from memory
gc()

#install.packages("mclust")

# read two libraries
library(cluster)
library(mclust)

# read data to clustering, info: http://archive.ics.uci.edu/ml/datasets/Wine
data_w <- read.table("wine.data", sep=",")

str(data_w)
summary(data_w)
head(data_w)
tail(data_w)
dim(data_w)

# first feature is real classification to 3 clusters
Wine <- data_w[,-1]
Real_class <- data_w[,1]

pairs(Wine, pch = 21, bg = Real_class) # not very meaningfull and effective
# way out of this problem is principal component analysis (PCA), technique of dimensionality reduction

pca_w <- prcomp(Wine, center = TRUE, scale. = TRUE)
attributes(pca_w)
plot(pca_w$x[,1:2], pch = 21, bg = Real_class, cex = 1.5, main = "Wine dataset")

# K-means
km_w <- kmeans(Wine, 3)
attributes(km_w)
km_w$cluster # classification to clusters
# plot results
layout(matrix(1:2, nrow=1, byrow=TRUE))
plot(pca_w$x[,1:2], pch = 21, bg = Real_class, cex = 1.5, main = "Wine dataset - real class.")
plot(pca_w$x[,1:2], pch = 21, bg = km_w$cluster, cex = 1.5, main = "Wine dataset - K-means")

# Model-based clustering
mbc_w <- Mclust(Wine, 3, modelNames = "EEE")
attributes(mbc_w)
mbc_w$classification
# plot results
layout(matrix(1:2, nrow=1, byrow=TRUE))
plot(pca_w$x[,1:2], pch = 21, bg = Real_class, cex = 1.5, main = "Wine dataset - real class.")
plot(pca_w$x[,1:2], pch = 21, bg = mbc_w$classification, cex = 1.5, main = "Wine dataset - MBC")

dev.off() # disable plot configuration - layout()
###############################################################################################################

## BONUS - data stream clustering ##

rm(list=ls())
gc()

library(stream)

stream <- DSD_Gaussians(k = 3, d = 2, noise = 0.05)
dbstream <- DSC_DBSTREAM(r = .05)
km <- DSC_Kmeans(k = 3, weighted = TRUE)

update(dbstream, stream, n = 1000)
dbstream
plot(dbstream, stream, type="both", main="DBStream")
recluster(km, dbstream)
km
plot(km, stream, type="both")

for(i in 1:5){
  update(dbstream, stream, n = 1000)
  print(dbstream)
  recluster(km, dbstream)
  print(km)
  plot(km, stream, type="both")
}
