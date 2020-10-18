emp <- read.csv("E:\\Datasets\\Simple linear regression\\emp_data.csv")
attach(emp)
View(emp)
#1st moment business decission
mean(Salary_hike)
mean(Churn_out_rate)
median(Salary_hike)
median(Churn_out_rate)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(Churn_out_rate)
getmode(Salary_hike)

#2nd moment business decission
sd(Churn_out_rate)
sd(Salary_hike)
var(Churn_out_rate)
var(Salary_hike)


#3rd moment business decission
library(moments)
skewness(Churn_out_rate)
skewness(Salary_hike)

#4th moment business decission
kurtosis(Churn_out_rate)
kurtosis(Salary_hike)

?hist
hist(Churn_out_rate)
hist(Salary_hike)
boxplot(Salary_hike,Churn_out_rate)
qqplot(Salary_hike,Churn_out_rate)
?cor
cor(emp)
install.packages("lattice")
library(lattice)
#Model
reg=lm(Churn_out_rate~Salary_hike,data = emp) #Y~X
##Model
reg=lm(Churn_out_rate~Salary_hike,data = emp) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
# log transformation
logST <-log(Salary_hike)
reg=lm(Churn_out_rate~logST,data = emp) 
summary(reg)
cor(logST,Churn_out_rate)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
#exponential Transformation
logDT <-log(Churn_out_rate)
reg=lm(logDT~Salary_hike,data = emp) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
cor(Churn_out_rate,logST)
cor(logDT,Salary_hike)
#square root Transformation
sqrrtDT <- sqrt(Churn_out_rate)
reg=lm(sqrrtDT~Salary_hike,data = emp) 
summary(reg)
confint(reg,level = 0.95)
predict(reg,interval = "predict")
cor(sqrrtDT,Churn_out_rate)


