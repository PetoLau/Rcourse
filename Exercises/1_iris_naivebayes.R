# IRIS - Naive Bayes classification ----

# load data
data(iris)
iris

# divide dataset to train and test part
id <- sample(1:nrow(iris), floor(nrow(iris)*0.8), replace = FALSE)
train <- iris[id,]
test <- iris[-id,]

# write datasets to files (optional)
# write.table(train, "train.csv", row.names = F, col.names = T)
# write.table(test, "test.csv", row.names = F, col.names = T)

# install/load package for NB
# install.packages("naivebayes")
library(naivebayes)

# Train model
# nb_m <- naive_bayes(train[,1:4], train[,5])
nb_m <- naive_bayes(Species ~ ., data = train)
nb_m

# Prediction on test set
pred <- predict(nb_m, test[,1:4])

# Evaluation
table(test$Species, pred)

# Accuracy
sum(diag(table(test$Species, pred)))/length(test$Species)

# plot the results ----
library(ggplot2)

ggplot(test, aes(Sepal.Length, Sepal.Width, color = Species, shape = Species)) +
  geom_point(alpha = 0.9, size = 4) +
  labs(title = "Real") +
  theme_bw()

ggplot(data.frame(test[,1:4], Species = pred), aes(Sepal.Length, Sepal.Width, color = Species, shape = Species)) +
  geom_point(alpha = 0.9, size = 4) +
  labs(title = "Naive Bayes classification") +
  theme_bw()
