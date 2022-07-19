data("airquality")
summary(airquality)

set.seed(123456)
# add more NA's
airquality$Solar.R[runif(nrow(airquality))<0.7] <-NA
airquality$Day[runif(nrow(airquality))<0.1] <-NA

# to se fully-obseved cases
comp<complete.cases(airquality)
mean(comp)

summary(lm(Ozone~.,data = airquality))

mod<-lm(Ozone~.,data = airquality)
modAIC <-MASS::stepAIC(mod)
# Akaike information criterion (AIC) is a 
# metric that is used to compare
# the fit of several regression models
AIC(lm(Ozone~.,data = airquality))
AIC(lm(Ozone~.,data = subset(airquality, select = -Solar.R)))
AIC(lm(Ozone~. -Solar.R, data = airquality))

airqualityNoNA <- na.exclude(airquality)
summary(airqualityNoNA)

# stepwise regresson without NA's
modBIC1 <- MASS::stepAIC(lm(Ozone~.,data = airqualityNoNA))
summary(modBIC1)

nrow(airqualityNoNA)/nrow(airquality)

# removing the predictor with many NA's
airqualityNoSolar.R <- na.exclude(subset(airquality,select = -Solar.R))
modBIC2 <- MASS::stepAIC(lm(Ozone~.,data = airqualityNoSolar.R), k = log(nrow(airqualityNoSolar.R)), trace = 0)
summary(modBIC2)

# imputing data using the sample mean
library(mice)
airqualityMean <-complete(mice(data = airquality, m = 1, method = "mean"))
airqualityLM <- complete(mice(data = airquality, m = 1, method = c("norm.predict", rep("mean",5))))

airqualityLM$Ozone[is.na(airquality$Ozone)]
predict(lm(airquality$Ozone~.,data = airqualityMean), newdata = airqualityMean[is.na(airquality$Ozone),-1])
