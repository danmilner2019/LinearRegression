dat <- read.csv('./Data/Data/indicator_sheet.csv')


x <- as.matrix(dat$LandCultivated)
x[is.na(x)] <- 0
y <- as.matrix(dat$farm_income)
y[is.na(y)] <- 0

#Using Land Cultivated to estimate Farm Income

X <- mean(as.numeric(dat$LandCultivated), na.rm = T)
Y <- mean(as.numeric(dat$farm_income), na.rm = T)

SdX <- sqrt(sum((x - X)^2, na.rm = T)/(nrow(dat)-1))
SdY <- sqrt(sum((y - Y)^2, na.rm = T)/(nrow(dat)- 1))

r = 1/(nrow(dat)-1) * sum(((x - X)/SdX) * ((y - Y)/SdY), na.rm=T)

solve(t(cbind(1,x)) %*%  cbind(1,x)) %*% (t(cbind(1,x)) %*% (y))



lm(dat$farm_income ~ dat$LandCultivated)
