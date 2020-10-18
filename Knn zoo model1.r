> Zoo = read.csv("E:\\Datasets\\KNN model\\Zoo.csv")
> attach(Zoo)
> View(Zoo)
> # drop the id feature
> Zoo <- Zoo[-1]
> # table of diagnosis
> table(Zoo$type)
> str(Zoo)
> # table or proportions with more informative labels
> round(prop.table(table(Zoo$type)) * 100, digits = 1)
> summary(Zoo[c(1:17)])
> # create training and test data
> wbcd_train <- Zoo[1:67, ]
> wbcd_test <- Zoo[68:101, ]
> wbcd_train_labels <- Zoo[1:67,17]
> wbcd_test_labels <- Zoo[68:101, 17]
> library(class)
> wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
+                       cl=wbcd_train_labels, k=1)
> tab <- table(wbcd_test_pred,wbcd_test_labels)
> accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
> accuracy(tab)
> #ACC.1 <- 100 * sum(wbcd_test_labels == wbcd_test_pred)/NROW(wbcd_test_labels)
> #ACC.1
> library(lattice)
> library(ggplot2)
> library(caret)
> confusionMatrix(table(wbcd_test_pred ,wbcd_test_labels))
> i=1
> k.optm=1
> for (i in 1:50){
+    knn.mod <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=i)
+    k.optm[i] <- 100 * sum(wbcd_test_labels == knn.mod)/NROW(wbcd_test_labels)
+    k=i
+    cat(k,'=',k.optm[i],'
+         ')
+ }
> plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
 
> # load the "gmodels" library
> library(gmodels)
> # Create the cross tabulation of predicted vs. actual
>  CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
+            prop.chisq=FALSE)

 

> # use the scale() function to z-score standardize a data frame
> Zoo_z <- as.data.frame(scale(Zoo))
> View(Zoo_z)
> # create training and test datasets
> wbcd_train <- Zoo_z[1:67, ]
> wbcd_test <- Zoo_z[68:101, ]
> # re-classify test cases
> wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
+                       cl = wbcd_train_labels, k=1)
> tab <- table(wbcd_test_pred,wbcd_test_labels)
> accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
> accuracy(tab)
> #ACC.1 <- 100 * sum(wbcd_test_labels == wbcd_test_pred)/NROW(wbcd_test_labels)
> #ACC.1
> library(lattice)
> library(ggplot2)
> library(caret)
> confusionMatrix(table(wbcd_test_pred ,wbcd_test_labels))
> i=1
> k.optm=1
> for (i in 1:50){
+   wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=i)
+   k.optm[i] <- 100 * sum(wbcd_test_labels == wbcd_test_pred)/NROW(wbcd_test_labels)
+   k=i
+   cat(k,'=',k.optm[i],'
+       ')
+ }      
> # Create the cross tabulation of predicted vs. actual
> CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
+            prop.chisq=FALSE)
> plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

 
> Zoo = read.csv("E:\\Datasets\\KNN model\\Zoo.csv")
> attach(Zoo)
> # table of diagnosis
> table(Zoo$type)
> str(Zoo)
> # drop the id feature
> Zoo <- Zoo[-1]
> str(Zoo)
> # table or proportions with more informative labels
> round(prop.table(table(Zoo$type)) * 100, digits = 1)
> # summarize any three numeric features
> summary(Zoo[c(1:16)])
> Zoo_n <- as.data.frame(Zoo[1:16])
> View(Zoo_n)
> # create training and test data
> wbcd_train <- Zoo_n[1:67, ]
> wbcd_test <- Zoo_n[68:101, ]
> wbcd_train_labels <- Zoo[1:67,17]
> wbcd_test_labels <- Zoo[68:101, 17]
> library(class)
> wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
+                       cl=wbcd_train_labels, k=1)
> tab <- table(wbcd_test_pred,wbcd_test_labels)
> accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
> accuracy(tab)
> #ACC.1 <- 100 * sum(wbcd_test_labels == wbcd_test_pred)/NROW(wbcd_test_labels)
> #ACC.1
> library(lattice)
> library(ggplot2)
> library(caret)
> confusionMatrix(table(wbcd_test_pred ,wbcd_test_labels))
> i=1
> k.optm=1
> for (i in 1:50){
+   knn.mod <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=i)
+   k.optm[i] <- 100 * sum(wbcd_test_labels == knn.mod)/NROW(wbcd_test_labels)
+   k=i
+   cat(k,'=',k.optm[i],'
+         ')
+ }        
> plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
 



