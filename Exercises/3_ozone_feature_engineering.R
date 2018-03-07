# Ozone - basic feature engineering ----
library(ggplot2)
library(GGally)
library(mgcv)
library(visreg)

ozone <- read.csv("Exercises/ozone.csv", sep = " ")

# O3 Daily maximum one-hour-average ozone reading
# wind Wind speed in LA airport (LAX)
# humidity Humidity at LAX
# temp Temperature 
# dpg Pressure gradient (mm Hg) from LAX to Daggett, CA
# vis Visibility (miles) measured at LAX
# doy Day of Year

# like a time series
ggplot(ozone, aes(doy, O3)) +
  geom_line() +
  theme_bw()

# Correlation matrix
cor(ozone) # Pearson
cor(ozone, method = "spearman")

# pairs - scatter plots
ggpairs(ozone[, -7], lower = list(continuous = wrap("smooth", method = "loess", color = "dodgerblue2")))

# Transformations of data
# normal?
qqnorm(ozone$O3)
qqline(ozone$O3, col = "red")

library(forecast)
power_lambda <- BoxCox.lambda(ozone$O3)
O3_boxcox <- BoxCox(ozone$O3, power_lambda)

qqnorm(O3_boxcox)
qqline(O3_boxcox, col = "red")

# Interactions between independent variables and its dependecy with O3
# Windchill?
ggplot(data.frame(O3 = O3_boxcox, wind_temp = ozone$wind*ozone$temp), aes(wind_temp, O3)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  annotate("text", x=70, y=5.5, label= paste("r =", round(cor(O3_boxcox, ozone$wind*ozone$temp),3)), size = 6) +
  theme_bw()

# humidity and tempurature - heat index
ggplot(data.frame(O3 = O3_boxcox, humidi_temp = ozone$humidity*ozone$temp), aes(humidi_temp, O3)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  annotate("text", x=1300, y=5.5, label= paste("r =", round(cor(O3_boxcox, ozone$humidity*ozone$temp),3)), size = 6) +
  theme_bw()

# some nonsense
ggplot(data.frame(O3 = O3_boxcox, dpg_vis = ozone$dpg*ozone$vis), aes(dpg_vis, O3)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  annotate("text", x=-13000, y=5, label= paste("r =", round(cor(O3_boxcox, ozone$dpg*ozone$vis),3)), size = 6) +
  theme_bw()

# non-linear trans. of independent var.
head(poly(ozone$temp, 2))
