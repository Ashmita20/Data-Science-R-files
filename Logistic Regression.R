install.packages("psych")
install.packages("caTools")
library(dplyr)
library(psych)
library(caTools)
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("broom")
library(broom)
library(tidyverse)
#Import dataset manually
bank <- bank.full
summary(bank)
describe(bank)
str(bank)
boxplot(bank)$outliers
colnames(bank)
bank$y[bank$y!='no'] <- 1
bank$y[bank$y=='no'] <- 0
bank <- na.omit(bank)
bank$y <- as.numeric(bank$y)
split <- sample.split(bank,SplitRatio = 0.8)
training <- subset(bank,split =="TRUE")
test <- subset(bank,split !="TRUE")
colnames(bank)
str(bank)
?glm
logit <- glm(y ~ age+factor(housing)+factor(education)+factor(marital)
             +balance+day+month+duration+campaign+pdays+previous,family='binomial',data=training[-c(29183,24149,44603)])

summary(logit)
car::vif(logit)
plot(logit)
plot(logit, which = 4, id.n = 3)
model.data <- augment(logit) %>% 
  mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
#Plot the standardized residuals:
  
  ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = "red"), alpha = .5) +
  theme_bw()
  model.data %>% 
    filter(abs(.std.resid) > 3)
logitr <- glm(y ~.,family='binomial',data=training)
summary(logitr)
#confusion matrix
pred1 <- predict(logitr,type = c("response"),test)
pred1
confusiontest <- table(test$y,pred1>0.5)
confusiontest

#model accuracy
accuracy <- sum(diag(confusiontest)/sum(confusiontest))
accuracy
exp(coef(logit))

logit <- glm(termdeposit ~ age+factor(housing)
             +balance+poutcome,family='binomial',data=bank)
summary(logit)


install.packages("ROCR")
library(ROCR)
pred2 <- predict(logitr,type = c("response"),training)
pred2
rocrpred <- prediction(pred2,training$y)
rocrpref <- performance(rocrpred,'tpr','fpr')
plot(rocrpref,colorize= T,print.cutoff=rocrpref@alpha.values[[1]])

#rocr cutoff
library(dplyr)
str(rocrpref)
rocr_cutoff <- data.frame(cutoff=rocrpref@alpha.values[[1]],fpr=rocrpref@x.values,tpr=rocrpref@y.values)
View(rocr_cutoff)

rocr_cutoff$cutoff <-round(rocr_cutoff$cutoff,4) 
colnames(rocr_cutoff  ) <- c("cutoff","fpr","tpr")
rocr_cutoff <-arrange( rocr_cutoff,desc(tpr))
View(rocr_cutoff)
rocr<- write.csv(rocr_cutoff,file="rocr.csv")
getwd()
