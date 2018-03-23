library(data.table)

housing <- fread("Exercises/housing.csv")

# colnames(housing) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO",
#                        "B", "LSTAT", "MEDV")
# 
# fwrite(housing, "housing.csv", col.names = TRUE, row.names = F, quote = F)

# 1. CRIM per capita crime rate by town
# 2. ZN proportion of residential land zoned for lots over 25,000 sq.ft.
# 3. INDUS     proportion of non-retail business acres per town
# 4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
# 5. NOX       nitric oxides concentration (parts per 10 million)
# 6. RM        average number of rooms per dwelling
# 7. AGE       proportion of owner-occupied units built prior to 1940
# 8. DIS       weighted distances to five Boston employment centres
# 9. RAD       index of accessibility to radial highways
# 10. TAX      full-value property-tax rate per $10,000
# 11. PTRATIO  pupil-teacher ratio by town
# 12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
# 13. LSTAT    % lower status of the population
# 14. MEDV Median value of owner-occupied homes in $1000's

summary(housing)
str(housing)

hist(housing$MEDV, breaks = 20)

housing$CHAS <- as.factor(housing$CHAS)

lm_1 <- lm(MEDV ~ ., data = housing)
summary(lm_1)
plot(lm_1)
