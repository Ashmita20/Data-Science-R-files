setwd("E:\\Datasets\\Hypothesis Testing")

##### Normality Test##################

library(readr)
library(readxl)
library(psych)

######## Read data ###################

Cutlets<-read.csv(file.choose())    # Cutlets.csv
attach(Cutlets)
colnames(Cutlets)<-c("Unit A","Unit B")
# Changing column names
View(Cutlets)
#attach(Cutlets)

Lab<-read.csv(file.choose())    # Lab.csv
attach(Lab)
colnames(Lab)<-c("Lab 1","Lab 2","Lab 3","Lab 4")
# Changing column names
View(Lab)
#attach(Cutlets)

Buyer<-read.csv(file.choose())    # BuyerRatio.csv
attach(Buyer)
colnames(Buyer)<-c("Gender","East","West","North","South")
# Changing column names
View(Buyer)

Customer<-read.csv(file.choose())    # Costomer+OrderForm.csv
attach(Customer)
Customer <- cbind(Phillippines,Indonesia,Malta,India)
# Changing column names
View(Customer)

Fantaloon<-read.csv(file.choose())    # Fantaloons.csv
attach(Fantaloon)
# Changing column names
View(Fantaloon)
#############Normality test###############
#Cultets 
shapiro.test(Unit.A)
# p-value = 0.32 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Unit.B)
# p-value = 0.5225 >0.05 so p high null fly => It follows normal distribution

#LabTAT
shapiro.test(Laboratory.1)
# p-value = 0.5508 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.2)
# p-value = 0.8637 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.3)
# p-value = 0.4205 >0.05 so p high null fly => It follows normal distribution

shapiro.test(Laboratory.4)
# p-value = 0.6619 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############
#Cutlets
var.test(Unit.A,Unit.B)#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances
#LabTAT
?var.test
?bartlett.test
bartlett.test(list(Laboratory.1,Laboratory.2,Laboratory.3,Laboratory.4))#variance test
# p-value = 0.3136 > 0.05 so p high null fly => Equal variances
############2 sample T Test ##################

t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4723 > 0.05 accept Null Hypothesis 
# unequal means

###################Proportional T Test(JohnyTalkers data)##########

Fantaloon<-read.csv(file.choose())   # Fantaloons.xlsx
View(Fantaloon) 
attach(Fantaloon)
table1 <-table(Weekdays,Weekend)
table1
?table()
?prop.test
library(gmodels)
CrossTable(x = Weekdays, y = Weekend,
                      prop.chisq=TRUE)
prop.test(x=c(167,66),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions of Male and Female under purchased
# p-value =  1.271e-10 < 0.05 reject Null hypothesis i.e.
# Unequal proportions 

prop.test(x=c(167,66),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "less")
# Ha -> Proportions of Female > Proportions of Male
# Ho -> Proportions of Male > Proportions of Female
# p-value = 1 >0.05 accept null hypothesis 
# so proportion of Female > proportion of Male 



#########Chi Square(Customer Order Form)#################

View(Customer) # countries are in their own columns; so we need to stack the data 
?stack
table(Customer)
stacked_Customer<-stack(Customer)
attach(stacked_Customer)
View(stacked_Customer)
table(stacked_Customer$ind,stacked_Customer$values)
chisq.test(table(stacked_Customer$ind,stacked_Customer$values))

#############Anova (Lab TAT)########## 

Stacked_Data <- stack(Lab)
View(Stacked_Data)
attach(Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 2e-16 < 0.05 reject null hypothesis 
# Atleast one Proportions is not equal 


# Customer order form 
# Unstacked data 

Buyer<-read.csv(file.choose()) # Buyer.csv
View(Buyer) # countries are in their own columns; so we need to stack the data 
?stack
str(Buyer)
table(Buyer)
library(MASS)       # load the MASS package 
tbl = table(Buyer$Gender,Buyer$East) 
tbl
chisq.test(tbl)
ctbl = cbind(tbl[,Buyer$West],tbl[,Buyer$North]+tbl[,Buyer$South]) 
ctbl
stacked_Buyer<-stack(Buyer$Gender,Buyer$East)
attach(stacked_Buyer)
View(stacked_Buyer)
table(Buyer$Gender,Buyer$East,Buyer$West,Buyer$North,Buyer$South)
?chisq.test
chisq.test(Buyer$Gender,Buyer$East,Buyer$West,Buyer$North,Buyer$South)


