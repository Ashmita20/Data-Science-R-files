startup <- read.csv("Cloud\\project\\Datasets\\50_Startups.csv")
attach(startup)
View(startup)
unique_states <- unique(startup$State)
unique(startup$State)
startup$State = factor( startup$State,
                        levels = c(unique_states),
                        labels = c(1, 2, 3))
#1st moment business decission
mean(RDSpend)
mean(Administration)
mean(MarketingSpend)
mean(Profit)
median(RDSpend)
median(Administration)
median(MarketingSpend)
median(Profit)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(RDSpend)
getmode(Administration)
getmode(MarketingSpend)
getmode(Profit)
#2nd moment business decission
sd(RDSpend)
sd(Administration)
sd(Profit)
sd(MarketingSpend)
var(RDSpend)
var(Administration)
var(MarketingSpend)
var(Profit)
#3rd moment business decision
#install.packages("moments")
library(moments)
skewness(RDSpend)
skewness(Administration)
skewness(MarketingSpend)
skewness(Profit)
#4th moment business decission
kurtosis(RDSpend)
kurtosis(Administration)
kurtosis(MarketingSpend)
kurtosis(Profit)
#Graphical representation
hist(RDSpend)
hist(Administration)
hist(MarketingSpend)
hist(Profit)
plot(RDSpend,Profit)
# linear and positive direction and strongly corelated
#Direction positive,linear
plot(Administration,Profit)
plot(MarketingSpend,Profit)
# can be said to be linear with positive direction and moderately corelated
plot(State)
#New York has outliers on both extreme
summary(X50_Startups)

#colinearity between RD spend and profit , marketing spend and rd spend ,marketing spend and profit

library(lattice)
library(moments)
install.packages("mlr")
install.packages("dummy")
library(fastDummies)
library(dummy)
library(mlr)
install.packages("knitr")
library(knitr)
#State_dummy <- dummy.code(X50_Startups$State,group=NULL)
#X50_Startups <-as.data.frame(cbind(X50_Startups,State_dummy))
#df1 <- as.data.frame(startup)
#results <- dummy_cols(df1)
#View(results)
#knitr::kable(results)
#results <- results[-4]
# fastDummies_example <- data.frame(numbers = 1:3,
#                                   gender  = c("male", "male", "female"),
#                                   animals = c("dog", "dog", "cat"),
#                                   dates   = as.Date(c("2012-01-01", "2011-12-31",
#                                                       "2012-01-01")),
#                                   stringsAsFactors = FALSE)

cor(startup)
pairs(startup)
#RD Spend with profit has strong corelation as value is r>0.85 
#RD Spend with Marketing spend has moderate corelation as value is <.85 but closer to it.
#Profit with Marketing spend has moderate corelation as value is closer to .85
#further strengthening the evidence that these are involved in colinearity problem
install.packages("corpcor")
library(corpcor)
## convert state column into numeric 
## convert state column into numeric 
startup$State <- as.numeric(as.character(startup$State))

#######Check is numeric column
is.numeric(startup$State)
is.numeric(startup$R.D.Spend)
is.numeric(startup$Administration)
is.numeric(startup$Marketing.Spend)
is.numeric(startup$Profit) 
cor2pcor(cor(startup))
#even partial corelation coefficent shows strong corelation between RD spend and profit
model_startup <- lm(Profit ~. , data=startup)
summary(model_startup)
install.packages("car")
library(car)
install.packages("carData")
library(carData)
model_startup <- lm(Profit ~ RDSpend+Administration+MarketingSpend , data=startup)
summary(model_startup)
graphics.off()
par(mar=c(2,2,2,2))
?influencePlot
influencePlot(model_startup,id.n=3)
vif(model_startup)
install.packages("MASS")
library(MASS)
stepAIC(model_startup)
plot(model_startup)
residualPlots(model_startup)
avPlots(model_startup)
qqPlot(model_startup)
influenceIndexPlot(model_startup)
model1<-lm(Profit ~ RDSpend+Administration+MarketingSpend,data=results[-c(49,50),])
summary(model1)
car::vif(model1)
library(MASS)
stepAIC(model1)
plot(model1)
residualPlots(model1)
avPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)
model2<-lm(Profit ~ RDSpend+MarketingSpend,data=startup)
summary(model2)
car::vif(model2)
library(MASS)
stepAIC(model2)
graphics.off()
par(mar=c(2,2,2,2))
plot(model2)
residualPlots(model2)
avPlots(model2)
qqplot(model2)
