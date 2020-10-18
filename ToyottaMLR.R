attach(ToyotaCorolla)
ToyotaCorolla <- read.csv("E:/Datasets/Multi linear Regression/ToyotaCorolla.csv")
View(ToyotaCorolla)
install.packages("e1071")
library(e1071)
install.packages("ggstatplot")
library(ggstatplot)
library(psych)
ToyotaCorolla<-ToyotaCorolla[,-c(1:2)]
View(ToyotaCorolla) 
str(ToyotaCorolla)
knitr::kable(summary(ToyotaCorolla))
summary(ToyotaCorolla)
describe(ToyotaCorolla)
View(ToyotaCorolla)
unique_fuel <- unique(ToyotaCorolla$Fuel_Type)
unique(ToyotaCorolla$Fuel_Type)
ToyotaCorolla$Fuel_Type = factor(ToyotaCorolla$Fuel_Type,
                          levels = c(unique_fuel),
                          labels = c(0,1,2))
unique_Color <- unique(ToyotaCorolla$Color)
unique(ToyotaCorolla$Color)
ToyotaCorolla$Color = factor(ToyotaCorolla$Color,
                                 levels = c(unique_Color),
                                 labels = c(0,1,2,3,4,5,6,7,8,9))
ToyotaCorolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
str(ToyotaCorolla)
boxplot(ToyotaCorolla)$out
Q <- quantile(ToyotaCorolla$KM, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(ToyotaCorolla$KM)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
Q1 <- quantile(ToyotaCorolla$Price, probs=c(.25, .75), na.rm = FALSE)
iqr1 <- IQR(ToyotaCorolla$Price)
up1 <-  Q1[2]+1.5*iqr1 # Upper Range  
low1<- Q1[1]-1.5*iqr1 # Lower Range
eliminated1<- subset(ToyotaCorolla, ToyotaCorolla$Price > (low1)& ToyotaCorolla$Price < (up1))
eliminated<- subset(eliminated1, eliminated1$KM > (low) & eliminated1$KM < (up))

boxplot(eliminated)$out
model <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = eliminated)
summary(model)

#model <- lm(Price ~ poly(Age_08_04,2)+KM+poly(HP,1)+cc+poly(Doors,1)+Gears+Quarterly_Tax+Weight, data = eliminated[-c(77,480,105,272,104,270),])
#summary(model)
graphics.off()
par(mar=c(1,1,1,1))
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
pairs(eliminated[,-c(6,9)])
cor(eliminated)
install.packages("car")
library(car)
car::vif(model)
library(MASS)
stepAIC(model)
plot(model)
residualPlots(model)
avPlots(model)  
qqPlot(model)
influenceIndexPlot(model)
View(eliminated)

model1<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+poly(Weight,2),data=eliminated[-c(193,222,77,106,394,602,269,476,268,473,471,267,961),])
summary(model1) 
car::vif(model1)
library(MASS)
stepAIC(model1)
?residualPlots
plot(model1)
residualPlots(model1)
avPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

