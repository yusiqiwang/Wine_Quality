rm(list = ls())
library(gam)
dat = read.csv('c:/users/du/Desktop/SPRING 2020/Business Intelligence/project1/winequality-red.csv',na.strings="?")


set.seed(1)
train = sample(1:nrow(dat), nrow(dat)/2)
Y = dat$quality
test = (-train)
Y.test = Y[test]
gam.X1 = gam(quality~ s(fixed.acidity,3)+s(volatile.acidity,3)+s(citric.acid,3)+s(residual.sugar,3)+s(chlorides,3)+s(free.sulfur.dioxide,3)+
               s(total.sulfur.dioxide,3)+s(density,3)+s(pH,3)+s(sulphates, 3)+s(alcohol,3), data=dat[train,])
#only s(sulphates, 3) is significent
gam.X2 = gam(quality ~ alcohol * fixed.acidity + alcohol * citric.acid + s(sulphates, 3), data=dat[train,])



summary(gam.X2)


R_squared_1 = 1-gam.X1$deviance/gam.X1$null.deviance
R_squared_2 = 1-gam.X2$deviance/gam.X2$null.deviance


quality_hat_1 = predict(gam.X1, newdata = dat[-train,])
quality_hat_2 = predict(gam.X2, newdata = dat[-train,])



MSE_1 = mean((quality_hat_1-Y.test)^2)
MSE_2 = mean((quality_hat_2-Y.test)^2)


#model 2 has least R_square and least mse in test set