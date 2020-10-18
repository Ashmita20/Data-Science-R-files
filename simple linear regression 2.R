delivery <- read.csv("E:\\Datasets\\Simple linear regression\\delivery_time.csv")
attach(delivery)
View(delivery)
#1st moment business decission
mean(delivery$Delivery.Time)
mean(delivery$Sorting.Time)
median(delivery$Delivery.Time)
median(delivery$Sorting.Time)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(delivery$Delivery.Time)
getmode(delivery$Sorting.Time)

#2nd moment business decission
sd(delivery$Delivery.Time)
sd(delivery$Sorting.Time)
var(delivery$Delivery.Time)
var(delivery$Sorting.Time)


#3rd moment business decission
library(moments)
skewness(delivery$Delivery.Time)
skewness(delivery$Sorting.Time)

#4th moment business decission
kurtosis(delivery$Delivery.Time)
kurtosis(delivery$Sorting.Time)

?hist
hist(delivery$Delivery.Time)
hist(delivery$Sorting.Time)
boxplot(delivery$Sorting.Time,delivery$Delivery.Time)
qqplot(delivery$Sorting.Time,delivery$Delivery.Time)
?cor
cor(delivery$Sorting.Time,delivery$Delivery.Time)
install.packages("lattice")
library(lattice)
#Model
reg=lm(Delivery.Time~Sorting.Time,data = delivery) #Y~X
##Model
reg=lm(Delivery.Time~Sorting.Time,data = delivery) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
# log transformation
logST <-log(delivery$Sorting.Time)
reg=lm(Delivery.Time~logST,data = delivery) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
#exponential Transformation
logDT <-log(delivery$Delivery.Time)
reg=lm(logDT~delivery$Sorting.Time,data = delivery) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
cor(delivery$Delivery.Time,logST)
cor(logDT,delivery$Sorting.Time)
#square root Transformation
sqrrtDT <- sqrt(delivery$Delivery.Time)
reg=lm(sqrrtDT~delivery$Sorting.Time,data = delivery) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
#log transformation
logDT <- log(delivery$Delivery.Time)
logST <- log(delivery$Sorting.Time)
reg=lm(logDT~logST,data = delivery) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
cor(logDT,logST)
qqplot(logDT,logST)
plot(logST,logDT)


?line
qqplot(logDT,delivery$Sorting.Time)

