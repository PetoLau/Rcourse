# Basic linear regression workflow ----
library(mgcv)
library(ggplot2)
library(visreg)

ozone <- read.csv("Exercises/ozone.csv", sep = " ")

# O3 Daily maximum one-hour-average ozone reading
# wind Wind speed in LA airport (LAX)
# humidity Humidity at LAX
# temp Temperature 
# dpg Pressure gradient (mm Hg) from LAX to Daggett, CA
# vis Visibility (miles) measured at LAX
# doy Day of Year

# GAM
gam_oz <- gam(O3 ~ s(wind) + s(humidity) + s(temp) + s(dpg) + s(vis) + s(doy),
              data = ozone)
summary(gam_oz)

visreg(gam_oz, "wind", gg=TRUE) + theme_bw()
visreg(gam_oz, "humidity", gg=TRUE) + theme_bw()
visreg(gam_oz, "temp", gg=TRUE) + theme_bw()
visreg(gam_oz, "dpg", gg=TRUE) + theme_bw()
visreg(gam_oz, "vis", gg=TRUE) + theme_bw()
visreg(gam_oz, "doy", gg=TRUE) + theme_bw()

# Linear regression - benchmark model
lm_1 <- lm(O3 ~ ., data = ozone)
summary(lm_1)
# wind unsignificant
plot(lm_1)

library(MASS)
stepAIC(lm_1)
# wind is missing

# based on GAM try polynomials
lm_2 <- lm(O3 ~ poly(temp, 2) + poly(humidity, 2) + 
             poly(dpg, 2) + poly(vis, 2) + poly(doy, 2), data = ozone)
summary(lm_2)
plot(lm_2)

stepAIC(lm_2)
# poly(humudity) has little impact

# try to find out interactions between independent variables
library(rpart)
rt_1 <- rpart(O3 ~ ., data = ozone, cp = 0.001)
plotcp(rt_1) # cp ~ 0.015
tr1 <- prune(rt_1, cp = 0.018)

library(rpart.plot)
rpart.plot(tr1, digits = 2, 
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0), 
           shadow.col = "grey65", col = "grey99")
# temp and humidity!

# let's see
ggplot(data.frame(O3 = ozone$O3, humidi_temp = ozone$humidity*ozone$temp), aes(humidi_temp, O3)) +
  geom_point(alpha = 0.8, size = 2.5) +
  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  annotate("text", x=1300, y=30, label= paste("r =", round(cor(ozone$O3, ozone$humidity*ozone$temp),3)), size = 6) +
  theme_bw()

# non-linear

# remove poly humidity and add poly interactions of humi and temp
lm_3 <- lm(O3 ~ temp:humidity + I(temp^2):I(humidity^2) + humidity +
             poly(dpg,2) + poly(doy, 2) + poly(vis,2),
           data = ozone)

summary(lm_3) # little improvement
plot(lm_3)

library(car)

slp(lm_3)
ncvTest(lm_3)

# transform O3
lm_4 <- lm(I(O3^(1/2)) ~ temp:humidity + I(temp^2):I(humidity^2) + humidity +
             poly(dpg,2) + poly(doy, 2) + poly(vis,2),
           data = ozone)

summary(lm_4) # nice improvement
stepAIC(lm_4)
ncvTest(lm_4) # still not significant

plot(studres(lm_4)) # library(MASS)
identify(1:330, studres(lm_4))
#[1]  21  53 258

outlierTest(lm_4) # just 258

# remove outliers
lm_5 <- lm(I(O3^(1/2)) ~ temp:humidity + I(temp^2):I(humidity^2) + humidity +
             poly(dpg,2) + poly(doy, 2) + poly(vis,2),
           data = ozone,
           subset = c(-21, -53, -258))

summary(lm_5) # improvement
stepAIC(lm_5)
ncvTest(lm_5)

ggplot(data = data.frame(Fitted_values = lm_5$fit,
                         Residuals = lm_5$res),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(title = "Fitted values vs Residuals")
# still not trully homosked

qqnorm(lm_5$res) # almost gaussian
qqline(as.vector(lm_5$res), col = "red")

rt_2 <- rpart(I(O3^(1/2)) ~ ., data = ozone, cp = 0.001)
plotcp(rt_2) # cp ~ 0.015
tr2 <- prune(rt_2, cp = 0.018)

rpart.plot(tr2, digits = 2,
           box.palette = viridis::viridis(10, option = "D", begin = 0.85, end = 0),
           shadow.col = "grey65", col = "grey99")
# temp and doy and temp and vis

lm_6 <- lm(I(O3^(1/2)) ~ temp:humidity + temp:doy + temp:vis + I(temp^2):I(humidity^2) + humidity +
             poly(dpg,2) + poly(doy, 2) + poly(vis,2),
           data = ozone,
           subset = c(-21, -53, -258))

summary(lm_6) # not really better

# try more advanced GAM
gam_oz_2 <- gam(I(O3^(1/2)) ~ s(wind) + t2(humidity, temp, full = TRUE) + s(humidity) + s(temp) + s(dpg) + s(vis) + s(doy), data = ozone)
summary(gam_oz_2)
# plot(gam_oz_2)

ggplot(data = data.frame(Fitted_values = gam_oz_2$fitted.values,
                         Residuals = gam_oz_2$residuals),
       aes(Fitted_values, Residuals)) +
  geom_point(size = 1.7) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  labs(title = "Fitted values vs Residuals")

qqnorm(gam_oz_2$res) # almost gaussian
qqline(as.vector(gam_oz_2$res), col = "red")

# feature importance by RF
library(randomForest)
rf_1 <- randomForest(I(O3^(1/2))~., data = ozone,
                     ntrees = 100, nodesize = 3, mtry = 4)
varImpPlot(rf_1)
