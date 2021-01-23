#knn
rm(list = ls())
library(class)
dat = read.csv('c:/users/du/Desktop/SPRING 2020/Business Intelligence/project1/winequality-red.csv',na.strings="?")
set.seed(1)
train = sample(1:nrow(dat), nrow(dat)/2)
dat_train = dat[train,]
dat_test = dat[-train,]
X_train = dat_train[,1:11]
Y_train = dat_train[,12]
X_test = dat_test[,1:11]
Y_test = dat_test[,12]
pred = knn(X_train, X_test, Y_train, k=1)
A = table(pred, Y_test)
accuracy = mean(pred == Y_test)
print(mean(pred == Y_test))