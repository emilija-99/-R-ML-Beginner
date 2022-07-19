a<-Advertising
library(tidyverse)
# test and train
set.seed(123)
smp_size<-floor(0.75*nrow(a))
train_ind<-sample(seq_len(nrow(a)), size = smp_size)
train<-a[train_ind,]
test<-a[-train_ind,]

# models
summary(train)

ltv<-lm(train$sales~train$TV)
summary(ltv)
gltv<-ggplot(data = train, mapping = aes(x = TV, y = sales))+geom_smooth()+geom_point()

graphics.off()
# TV NP
# produce residual vs. fitted plot
res.tv<-resid(ltv.lnp)
plot(fitted(ltv.lnp),res.tv)+
abline(0,0)
# produce a q-q plot
qqnorm(res.tv)
qqline(res.tv)

fitted = predict(ltv,as.data.frame(train["TV"]))
summary(fitted)
# TV R
# produce residual vs. fitted plot
res.tv.r<-resid(ltv.lr)
plot(fitted(ltv.lr),res.tv.r)+
  abline(0,0)
# produce a q-q plot
qqnorm(res.tv.r)
qqline(res.tv.r)

lnp<-lm(train$sales~train$newspaper)
summary(lnp)
gnp<-ggplot(data = train, mapping = aes(x = newspaper, y = sales))+geom_smooth()+geom_point()

lr<-lm(train$sales~train$radio)
summary(lr)
gr<-ggplot(data = train, mapping = aes(x =radio, y = sales))+geom_smooth()+geom_point()

par(mfrow = c(3,1))
ggplot(data = train, mapping = aes(x = TV, y = sales))+geom_smooth()+geom_point()
ggplot(data = train, mapping = aes(x = newspaper, y = sales))+geom_smooth()+geom_point()
ggplot(data = train, mapping = aes(x =radio, y = sales))+geom_smooth()+geom_point()

# linear regresion
lm.all<-lm(train$sales~.,data = train)
summary(lm.all)
par(mfrow = c(2,2))
plot(lm.all)
ltv.lnp <-lm(train$sales~train$TV +train$newspaper)
summary(ltv.lnp)

ltv.lr <-lm(train$sales~train$radio+train$TV)
summary(ltv.lr)

# levkasti oblik <- newspaper
lr.lnp <-lm(train$sales~train$radio+train$newspaper)
summary(lr.lnp)
gnp<-ggplot(data = train, mapping = aes(x = newspaper, y = sales))+geom_point()
gnp+scale_y_sqrt()

ltv.lnp.lr <-lm(train$sales~train$TV+train$radio+train$newspaper)
summary(ltv.lnp.lr)
model_sinergy<-lm(sales~TV*radio, data = train)
summary(model_sinergy)

# polinom_regresion
pol_tv_np<-lm(train$sales ~ train$TV+train$newspaper^2)
summary(pol_tv_np)
plot(pol_tv_np)

# tacke koje mogu biti uticajne 95,129,99


