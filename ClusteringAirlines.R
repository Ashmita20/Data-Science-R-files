install.packages("ggplot2")
library(ggplot2)
EastWestAirlines <- read.csv("E:\\Datasets\\KmeansClustering\\EastWestAirlines.csv")
Airlines <- EastWestAirlines[-1]
summary(Airlines)
library(psych)
describe(Airlines)
str(Airlines)
Na <- is.na(Airlines)
table(Na)
Airlines <-na.omit(Airlines)
#standardize <- function(x){(x-min(x))/(max(x)-min(x))}
# normalize the data excluding the X
z<- Airlines[]
m<- apply(z,2,mean)
s<- apply(z,2,sd)
z<- scale(z,m,s)
summary(z)
#Agglomerative Clustering
#This is a "bottom-up" approach: each observation starts in its own cluster, and pairs of clusters are merged as one moves up the hierarchy.
library(cluster)
h_cluster <- agnes(z, method = 'ward', metric = 'euclidean')
plot(h_cluster)
#Hclust with Manhattan distance measure
d<-dist(z,method="manhattan")
fit <- hclust(d,method="average")
plot(fit)
plot(fit,labels=EastWestAirlines$ID,hang=-1)
#Hclust with Mahalanobis distance measure
install.packages("StatMatch")
library(StatMatch)
M<-mahalanobis.dist(z, data.y=NULL, vc=NULL)
fitM <- hclust(M,method="single")
plot(fitM)
plot(fitM,labels=EastWestAirlines$ID,hang=-1)
#DBSCAN clustering
install.packages("dbscan")
 library("dbscan")
kNNdistplot(Airlines, k =5)
abline(h=.05, col = "red", lty=2)
res <- dbscan(Airlines, eps = 3800, minPts = 12)
res$cluster
res$eps
# applying sqrt n/2 formula ,k=44
groups <- cutree(fit,k=44)
membership<-as.matrix(res$cluster) # groups or cluster numbers
final <- data.frame(EastWestAirlines, membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
?write.csv
write.csv(final1, file="EastWestAirlinesH.csv",row.names = F)
getwd()
?aggregate
aggregate(EastWestAirlines[,-1],by=list(final$membership),mean)
