install.packages("ggplot2")
library(ggplot2)
summary(crime_data)
normalize <- scale(crime_data[,2:5])
summary(normalize)
d<-dist(normalize,method="euclidean")
d
fit <- hclust(d,method="complete")
plot(fit,labels=crime_data$X)
plot(fit,labels=crime_data$X,hang=-1)
?cutree
#No of clusters formula is sqrt n/2 where n is number of rows
groups <- cutree(fit,k=5)
groups
rect.hclust(fit,k=5,border="red")
?rect.hclust
?as.matrix
membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime_data, membership)
View(final)
final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
?write.csv
write.csv(final1, file="final.csv",row.names = F)
getwd()
?aggregate
aggregate(crime_data[,-1],by=list(final$membership),mean)
