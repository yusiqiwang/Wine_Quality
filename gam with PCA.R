rm(list = ls())
library(gam)
library(psych)

dat = read.csv('c:/users/du/Desktop/SPRING 2020/Business Intelligence/project1/winequality-red.csv',na.strings="?")

fa.parallel(dat, fa = 'pc', n.iter = 100, show.legend = FALSE)

princomp2.pr = princomp(~., data=dat[,1:11], cor=T)

summary(princomp2.pr, loadings=TRUE)

pre = predict(princomp2.pr)
princomp2 = dat
princomp2$z1 = pre[,1]
princomp2$z2 = pre[,2]
princomp2$z3 = pre[,3]
princomp2$z4 = pre[,4]
princomp2$z5 = pre[,5]
princomp2$z6 = pre[,6]

set.seed(3)
train = sample(1:nrow(princomp2), nrow(princomp2)/2)
Y = dat$quality
test = (-train)
Y.test = Y[test]

lm.X1 = lm(quality~ z1+z2+z3+z4+z5+z6, data=princomp2[train,])

gam.X1 = gam(quality~ s(z1,4)+s(z2,4)+s(z3,4)+s(z4,4)+s(z5,4)+s(z6,4), data=princomp2[train,])
summary(gam.X1)

gam.X2 = gam(quality~ z1+z2+z3+s(z4,4)+z5+z6, data=princomp2[train,])
summary(gam.X2)

gam.X3 = gam(quality~ z1+z2+z3+s(z4,4)+z5, data=princomp2[train,])
summary(gam.X3)

gam.X4 = gam(quality~ z1+z2+z3+s(z4,4)+z5+z1:z3, data=princomp2[train,])
summary(gam.X4)


MSE_lm=mean((predict(lm.X1, newdata = princomp2[-train,] )- Y.test)^2)

MSE_gam1=mean((predict(gam.X1, newdata = princomp2[-train,] )- Y.test)^2)
MSE_gam2=mean((predict(gam.X2, newdata = princomp2[-train,] )- Y.test)^2)
MSE_gam3=mean((predict(gam.X3, newdata = princomp2[-train,] )- Y.test)^2)
MSE_gam4=mean((predict(gam.X4, newdata = princomp2[-train,] )- Y.test)^2)