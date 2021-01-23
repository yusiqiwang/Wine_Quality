
dat = read.csv('c:/users/du/Desktop/SPRING 2020/Business Intelligence/project1/winequality-red.csv',na.strings="?")

#test nan values
sum(is.na(dat))

cor(dat)
plot(dat)

#scatter of each feature
plot(dat$volatile.acidity)
plot(dat$fixed.acidity)
plot(dat$citric.acid)
plot(dat$residual.sugar)
plot(dat$chlorides)
plot(dat$free.sulfur.dioxide)
plot(dat$total.sulfur.dioxide)
plot(dat$density)
plot(dat$pH)
plot(dat$sulphates)
plot(dat$alcohol)

# test outlier
lm.X = lm(quality~. , data=dat)
plot(lm.X)

#try to find some intetractions
plot(dat[dat$alcohol==11,]$pH, dat[dat$alcohol==11,]$quality)
abline(lm(quality~pH,dat[dat$alcohol==11,]))
plot(dat[dat$alcohol==9,]$pH, dat[dat$alcohol==9,]$quality)
abline(lm(quality~pH, dat[dat$alcohol==9,]))

plot(dat[dat$alcohol==11,]$volatile.acidity, dat[dat$alcohol==11,]$quality)
abline(lm(quality~volatile.acidity, dat[dat$alcohol==11,]))
plot(dat[dat$alcohol==9,]$volatile.acidity, dat[dat$alcohol==9,]$quality)
abline(lm(quality~volatile.acidity, dat[dat$alcohol==9,]))

plot(dat[dat$alcohol==11,]$residual.sugar, dat[dat$alcohol==11,]$quality)
abline(lm(quality~residual.sugar, dat[dat$alcohol==11,]))
plot(dat[dat$alcohol==9,]$residual.sugar, dat[dat$alcohol==9,]$quality)
abline(lm(quality~residual.sugar, dat[dat$alcohol==9,]))

plot(dat[dat$alcohol==11,]$chlorides, dat[dat$alcohol==11,]$quality)
abline(lm(quality~chlorides, dat[dat$alcohol==11,]))
plot(dat[dat$alcohol==9,]$chlorides, dat[dat$alcohol==9,]$quality)
abline(lm(quality~chlorides, dat[dat$alcohol==9,]))