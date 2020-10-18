library(ggplot2)
library(dplyr)
library(MASS)
library(car)
library(caTools)
library(stats)
library(ggfortify)
EastWestAirlines <- read.csv("E:\\Datasets\\KmeansClustering\\EastWestAirlines.csv")
Airlines <- EastWestAirlines[-1]
colnames(EastWestAirlines1)[12] <- "Award"
colnames(EastWestAirlines1)[1] <- "ID"
Airlines <- na.omit(EastWestAirlines)
data<- scale(Airlines[,-1])
View(Airlines)
str(Airlines)
wss<- (nrow(data)-1) * sum(apply(data,2,var))
(nrow(data)-1)
?apply
sum(apply(data,2,var))
#lesser the within ss better the plot and more the between ss better the plot
# total within ss is sum of wss
# total ss is sum of between ss and within ss
#elbow method implementation 1
wssplot <- function(data,nc=15,seed =1234){
  wss<- (nrow(data)-1) * sum(apply(data,2,var)) 
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
    print (i)
    print(wss[i])
  }
  plot(1:nc,wss,type="b",xlab ="Number of clusters",ylab="Within groups sum of squares",title="Elbow Scree plot")
}
wssplot(Airlines)
install.packages("factoextra")
#elbow method implementation 2
library(factoextra)
fviz_nbclust(EastWestAirlines,kmeans,method="wss")+labs(subtitle ='Elbow Method')
km <- kmeans(EastWestAirlines,4)
str(km)
View(EastWestAirlines)
autoplot(km,EastWestAirlines,frame=TRUE)
print(km)
final <-data.frame(Airlines,km$cluster)
finalAirlines <- final[,c(ncol(final),1:(ncol(final)-1))]
colnames(finalAirlines)[1]<- "Cluster Number"
write.csv(finalAirlines,file="FinalAirlines.csv",row.names = F)
getwd()
km$size
install.packages("kselection")
library(kselection)
library(parallel)
registerDoParallel
k <-kselection(Airlines[,-1],parallel = TRUE,k_threshold = 0.9,max_centers = 15)
k
?kselection
plot(k)
#clustering Large Application
library(cluster)
xds<- rbind(cbind(rnorm(5000,0,8),rnorm(5000,0,8)),cbind(rnorm(5000,50,8),rnorm(5000,50,8)))
xcl <- clara(xds,2,sample =100)
clusplot(xcl)
#patitioning around mediods
xpm <-pan(xds,2)
clusplot(xpm)
# Animation plot
install.packages("animation")
library(animation)
kani <- kmeans.ani(Airlines,4)
