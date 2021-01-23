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
gam.X2 = gam(quality~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+s(sulphates, 3)+alcohol, data=dat[train,])

gam.X3 = gam(quality~ fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+s(sulphates, 3)+alcohol, data=dat[train,])

gam.X4 = gam(quality~ fixed.acidity+volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+s(sulphates, 3)+alcohol, data=dat[train,])

gam.X5 = gam(quality~ fixed.acidity+volatile.acidity+chlorides+total.sulfur.dioxide+density+pH+s(sulphates, 3)+alcohol, data=dat[train,])

gam.X6 = gam(quality~ fixed.acidity+volatile.acidity+chlorides+total.sulfur.dioxide+density+s(sulphates, 3)+alcohol, data=dat[train,])

gam.X7 = gam(quality~ fixed.acidity+volatile.acidity+density+total.sulfur.dioxide+s(sulphates, 3)+alcohol, data=dat[train,])

gam.X8 = gam(quality~ fixed.acidity+volatile.acidity+density+s(sulphates, 3)+alcohol, data=dat[train,])

summary(gam.X5)
summary(gam.X6)
summary(gam.X7)
summary(gam.X8)

R_squared_1 = 1-gam.X1$deviance/gam.X1$null.deviance
R_squared_2 = 1-gam.X2$deviance/gam.X2$null.deviance
R_squared_3 = 1-gam.X3$deviance/gam.X3$null.deviance
R_squared_4 = 1-gam.X4$deviance/gam.X4$null.deviance
R_squared_5 = 1-gam.X5$deviance/gam.X5$null.deviance
R_squared_6 = 1-gam.X6$deviance/gam.X6$null.deviance
R_squared_7 = 1-gam.X7$deviance/gam.X7$null.deviance
R_squared_8 = 1-gam.X8$deviance/gam.X8$null.deviance

quality_hat_1 = predict(gam.X1, newdata = dat[-train,])
quality_hat_2 = predict(gam.X2, newdata = dat[-train,])
quality_hat_3 = predict(gam.X3, newdata = dat[-train,])
quality_hat_4 = predict(gam.X4, newdata = dat[-train,])
quality_hat_5 = predict(gam.X5, newdata = dat[-train,])
quality_hat_6 = predict(gam.X6, newdata = dat[-train,])
quality_hat_7 = predict(gam.X7, newdata = dat[-train,])
quality_hat_8 = predict(gam.X8, newdata = dat[-train,])


MSE_1 = mean((quality_hat_1-Y.test)^2)
MSE_2 = mean((quality_hat_2-Y.test)^2)
MSE_3 = mean((quality_hat_3-Y.test)^2)
MSE_4 = mean((quality_hat_4-Y.test)^2)
MSE_5 = mean((quality_hat_5-Y.test)^2)
MSE_6 = mean((quality_hat_6-Y.test)^2)
MSE_7 = mean((quality_hat_7-Y.test)^2)
MSE_8 = mean((quality_hat_8-Y.test)^2)


#model 8 has least R_square and least mse in test set
