# Titanic classification task ----
library(data.table)
library(ggplot2)

train <- fread("titanic.csv", na.strings = c("", " "))
test <- fread("titanic_test.csv", na.strings = c("", " "))
test[, Survived := NA]

data_all <- rbindlist(list(train, test), use.names = TRUE)

summary(data_all)
summary(train)
summary(test)
str(train)

table(train$Survived)
# table(train$Cabin)
table(train$Sex)
table(train$Embarked)

train$Name[1]

# Generate Title feature ----
data_all[, Title := sapply(Name, function(x) strsplit(x, split='[,.]')[[1]][2])]
table(data_all$Title)
data_all[, Title := sub(' ', '', Title)]
table(data_all$Title)
data_all[Title %in% c('Mme', 'Mlle'), "Title"] <- 'Mrs'
table(data_all$Title)
data_all[Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Dr', 'Rev'), "Title"] <- 'Sir'
data_all[Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer'), "Title"] <- 'Miss'
table(data_all$Title)
data_all[Title %in% c('Col', 'Ms'), "Title"] <- 'Mr'
table(data_all$Title)

data_all[, Title := factor(Title)]

table(data_all$Survived, data_all$Title)
chisq.test(table(data_all$Survived, data_all$Title)) # p-value < 0.05

# Generate Family size feature ----
summary(data_all)
data_all[, FamilySize := SibSp + Parch + 1]
table(data_all$FamilySize)

wilcox.test(data_all[Survived == 1, FamilySize], data_all[Survived == 0, FamilySize], paired = F)

# Create Is.Alone feature ----
data_all[, IsAlone := factor(ifelse(FamilySize == 1, 1, 0))]
table(data_all$IsAlone)
fisher.test(table(data_all$IsAlone, data_all$Survived)) # p-value < 0.05

# transform categorical to factor ----
data_all[, Sex := factor(Sex)]
data_all[, Embarked := factor(Embarked)]
data_all[, Pclass := factor(Pclass)]
data_all[, Survived := factor(Survived)]

data_all[, Cabin := factor(Cabin)]

str(data_all)
summary(data_all)

# Embarked has 2 NAs
data_all <- data_all[!is.na(Embarked)]
summary(data_all)
data_all[is.na(Fare), "Fare"] <- median(data_all$Fare, na.rm = TRUE)
summary(data_all)

# split again on test and train
train <- data_all[1:889]
test <- data_all[890:.N]

# First model ----
glm_1 <- glm(Survived ~ Pclass + Sex + Fare + Embarked + Title + FamilySize + IsAlone, data = train,
             family = binomial(link = "logit"))

summary(glm_1)

library(MASS)
stepAIC(glm_1)

# predict on train set
pred_1 <- predict(glm_1, train, type = "response")
pred_1 <- ifelse(pred_1 > 0.55, 1, 0) # play with treshold!
table(pred_1, train$Survived)
sum(diag(table(pred_1, train$Survived))) / nrow(train)

# removing Embarked feature ----
glm_2 <- glm(Survived ~ Pclass + Sex + Fare + Title + FamilySize + IsAlone, data = train,
             family = binomial(link = "logit"))

summary(glm_2)

pred_2 <- predict(glm_2, train, type = "response")
pred_2 <- ifelse(pred_2 > 0.55, 1, 0)
table(pred_2, train$Survived)
sum(diag(table(pred_2, train$Survived))) / nrow(train)

# polynomials ----
glm_3 <- glm(Survived ~ Pclass + Sex + poly(Fare,3) + Title + poly(FamilySize,3), data = train,
             family = binomial(link = "logit"))

summary(glm_3)
stepAIC(glm_3)

pred_3 <- predict(glm_3, train, type = "response")
pred_3 <- ifelse(pred_3 > 0.55, 1, 0)
table(pred_3, train$Survived)
sum(diag(table(pred_3, train$Survived))) / nrow(train)

# create submission
# Prediction <- predict(glm_3, test, type = "response")
# Prediction <- ifelse(Prediction > 0.5, 1, 0)
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "fea_eng_glm_3.csv", row.names = FALSE)

# 0.78947 score on test set

# inpute age by model ----
age_train <- data_all[!is.na(Age)]
age_test <- data_all[is.na(Age)]
# str(age_train)

ggplot(age_train, aes(Age)) +
  geom_density() +
  theme_bw()
summary(age_train$Age)

# Poisson - discrete values! Need to preprocess Age to integer
age_model <- glm(floor(Age) ~ Pclass + Sex + Title + SibSp + Parch + IsAlone, data = age_train,
                 family = poisson(link = "log"))

summary(age_model)
plot(age_model)

predicted_age <- predict(age_model, age_test)
age_test$Age <- exp(predicted_age)
summary(age_test)

ggplot(age_test, aes(Age)) +
  geom_density() +
  theme_bw()

data_all[is.na(Age), "Age"] <- predicted_age
summary(data_all)
train <- data_all[1:889]
test <- data_all[890:.N]

# interactions Sex*Age, Pclass*Fare ----

ggplot(data_all, aes(Fare)) +
  facet_grid(Pclass~.) +
  geom_density()

glm_4 <- glm(Survived ~ Pclass + Sex + IsAlone:Sex + IsAlone:Age + Age + Age:Sex + poly(Fare,3) + Pclass:Fare + Title + FamilySize + IsAlone, data = train,
             family = binomial(link = "logit"))

summary(glm_4)
stepAIC(glm_4)

pred_4 <- predict(glm_4, train, type = "response")
pred_4 <- ifelse(pred_4 > 0.50, 1, 0)
table(pred_4, train$Survived)
sum(diag(table(pred_4, train$Survived))) / nrow(train)

# PCA ----
# library(Rtsne)
# tsne_mat <- Rtsne(scale(data.matrix(train[!duplicated(train[, .(Sex, Pclass, Age, Fare, Title, FamilySize)]),
#                                     .(Sex, Pclass, Age, Fare, Title, FamilySize)])),
#                   dims = 2, initial_dims = 6, pca = F, max_iter = 500, perplexity = 30)
# 
# plot(tsne_mat$Y, pch = 19, col = train$Survived[!duplicated(train[, .(Sex, Pclass, Age, Fare, Title, FamilySize)])])

pca_train <- prcomp(data.matrix(train[, .(Age, Fare, FamilySize)]),
                    center = TRUE, scale. = TRUE)
plot(pca_train$x[, 1:2], pch = 19, col = train$Survived)
plot(pca_train$x[, 1:2], pch = 19, col = pred_4+1)

# ManorNot
data_all[, ManorNot := ifelse(Age > 12 & Sex == "male", 1, 0)]
table(data_all$ManorNot)
table(data_all$Survived, data_all$ManorNot)
fisher.test(table(data_all$Survived, data_all$ManorNot))

data_all[, ManorNot := factor(ManorNot)]
train <- data_all[1:889]
test <- data_all[890:.N]

# ManorNot and remove Sex ----
glm_5 <- glm(Survived ~ Pclass + Age + ManorNot + poly(Fare,3) + Title + FamilySize + IsAlone, data = train,
             family = binomial(link = "logit"))

summary(glm_5)
stepAIC(glm_5)

pred_5 <- predict(glm_5, train, type = "response")
pred_5 <- ifelse(pred_5 > 0.55, 1, 0)
table(pred_5, train$Survived)
sum(diag(table(pred_5, train$Survived))) / nrow(train)

# binning of numeric variables ----
library(rattle)

data_all[, AgeBin := binning(Age, bins = 5, method = "kmeans")]
data_all[, FareBin := binning(Fare, bins = 5, method = "kmeans")]
levels(data_all$AgeBin)
levels(data_all$FareBin)
# str(data_all)
train <- data_all[1:889]
test <- data_all[890:.N]

glm_6 <- glm(Survived ~ Pclass + AgeBin + ManorNot + FareBin + Title + FamilySize + IsAlone, data = train,
             family = binomial(link = "logit"))

summary(glm_6)
stepAIC(glm_6)

pred_6 <- predict(glm_6, train, type = "response")
pred_6 <- ifelse(pred_6 > 0.55, 1, 0)
table(pred_6, train$Survived)
sum(diag(table(pred_6, train$Survived))) / nrow(train)

glm_7 <- glm(Survived ~ Pclass + AgeBin + poly(Fare, 3) + Title + FamilySize + Parch, data = train,
             family = binomial(link = "logit"))

summary(glm_7)
stepAIC(glm_7)

pred_7 <- predict(glm_7, train, type = "response")
pred_7 <- ifelse(pred_7 > 0.55, 1, 0)
table(pred_7, train$Survived)
sum(diag(table(pred_7, train$Survived))) / nrow(train)

# outliers ----
library(car)
outlierTest(glm_7) # 622

plot(cooks.distance(glm_7))
abline(h = 4/(nrow(train)-length(glm_7$coefficients)-1), lty = 2, lwd = 2, col = "red") # 4/(n - k - 1) is the rule
identify(1:nrow(train), cooks.distance(glm_7))
# 26  68 233 297 390 449 498 570 599 660 766 796 822

glm_8 <- glm(Survived ~ Pclass + AgeBin + poly(Fare, 3) + Title + FamilySize + Parch, data = train,
             family = binomial(link = "logit"), subset = -c(622,26,68,233,297,390,449,498,570,599,660,766,796,822))

summary(glm_8) # decrease of AIC - yey!
stepAIC(glm_8)
# plot(glm_8)

pred_8 <- predict(glm_8, train[-c(622,26,68,233,297,390,449,498,570,599,660,766,796,822)], type = "response")
pred_8 <- ifelse(pred_8 > 0.55, 1, 0)
table(pred_8, train$Survived[-c(622,26,68,233,297,390,449,498,570,599,660,766,796,822)])
sum(diag(table(pred_8, train$Survived[-c(622,26,68,233,297,390,449,498,570,599,660,766,796,822)]))) / (nrow(train) -14)

# create submission
Prediction <- predict(glm_8, test, type = "response")
Prediction <- ifelse(Prediction > 0.55, 1, 0)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "fea_eng_glm_8_outliers.csv", row.names = FALSE)

# try some hammer on it
library(randomForest)
rf_1 <- randomForest(Survived ~ Pclass + AgeBin + Fare + ManorNot + Title + FamilySize + IsAlone, data = train,
                     ntree = 500, nodesize = 3, mtry = 5, importance = TRUE)

varImpPlot(rf_1)
pred_rf <- predict(rf_1, train, type = "class")
table(pred_rf, train$Survived)
sum(diag(table(pred_rf, train$Survived))) / nrow(train)

# create submission
# Prediction <- predict(rf_1, test, type = "prob")
# Prediction <- ifelse(Prediction[,2] > 0.55, 1, 0)
# table(Prediction)
# submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "fea_eng_rf_4.csv", row.names = FALSE)

# 0.76076, with treshold 0.78468

# BONUS ----
# catboost

# library(catboost)
# 
# summary(train)
# data_all[, AgeBin := factor(as.character(AgeBin))]
# cols <- unlist(sapply(data_all[,.(AgeBin, Fare, ManorNot, IsAlone, Title, FamilySize, Pclass)],class))
# fac_cols <- which(cols == "factor")
# data_all[, (names(fac_cols)) := lapply(.SD, as.integer), .SDcols = names(fac_cols)]
# 
# train <- data_all[1:889]
# test <- data_all[890:.N]
# 
# train[, Survived := ifelse(Survived == "0", 0, 1)]
# 
# train_pool <- catboost.load_pool(as.matrix(train[,.(AgeBin, Fare, ManorNot, IsAlone, Title, FamilySize, Pclass)]),
#                                  label = as.matrix(train[,Survived]),
#                                  cat_features = c(1,3,4,5,7)-1)
# 
# test_pool <- catboost.load_pool(as.matrix(test[,.(AgeBin, Fare, ManorNot, IsAlone, Title, FamilySize, Pclass)]),
#                                 cat_features = c(1,3,4,5,7)-1)
# 
# # parameters tuning
# fit_params <- list(iterations = 400,
#                    learning_rate = 0.02,
#                    depth = 10,
#                    rsm = 0.8,
#                    l2_leaf_reg = 2.5,
#                    train_dir = "train_dir",
#                    boosting_type = "Dynamic",
#                    bootstrap_type = "Bernoulli",
#                    subsample = 0.8,
#                    sampling_frequency = "PerTreeLevel",
#                    thread_count = 4,
#                    loss_function = 'Logloss:Border=0.5',
#                    eval_metric = "Logloss",
#                    one_hot_max_size = 8,
#                    logging_level = "Silent",
#                    leaf_estimation_method = "Gradient",
#                    max_ctr_complexity = 5
#                    )
# 
# m_cat <- catboost.train(train_pool, params = fit_params)
# pred_cat_Train <- catboost.predict(m_cat, train_pool, prediction_type = "Class")
# table(pred_cat_Train, train$Survived)
# sum(diag(table(pred_cat_Train, train$Survived))) / nrow(train)
# 
# # make submission
# pred_cat <- catboost.predict(m_cat, test_pool, prediction_type = "Class")
# table(pred_cat)
# submit <- data.frame(PassengerId = test$PassengerId, Survived = pred_cat)
# write.csv(submit, file = "catboost_3.csv", row.names = FALSE)
