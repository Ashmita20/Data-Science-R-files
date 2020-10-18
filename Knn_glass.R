 glass = read.csv("E:\\Datasets\\KNN model\\glass.csv")
 attach(glass)
 View(glass)
   # table of diagnosis
     table(glass$Type)
   str(glass)
   # table or proportions with more informative labels
     round(prop.table(table(glass$Type)) * 100, digits = 1)
   summary(glass[c(1:9)])
   normalize <- function(x) {
     return ((x - min(x)) / (max(x) - min(x)))
   }
   # normalize the glass data
   glass_n <- as.data.frame(lapply(glass[1:9], normalize))
   summary(glass_n[c(1:9)])
   set.seed(123)
   dat.d <- sample(1:nrow(glass_n),size=nrow(glass_n)*0.7,replace = FALSE) #random selection of 70% data.
   
   glass_train <- glass_n[dat.d,] # 70% training data
   glass_test <- glass_n[-dat.d,] # remaining 30% test data
     #glass_train <- glass_n[1:150, ]
     #glass_test <- glass_n[151:214, ]
     View(glass_train)
     View(glass_test)
     glass_train_labels <- glass[dat.d,10]
     glass_test_labels <- glass[-dat.d,10]
     print(glass_train_labels)
     print(glass_test_labels)
     library(class)
     NROW(glass_train_labels)
     glass_test_pred12 <- knn(train=glass_train, test=glass_test,
                            cl=glass_train_labels, k=12)
     
     glass_test_pred13 <- knn(train=glass_train, test=glass_test,
                                                 cl=glass_train_labels, k=13)
     tab <- table(glass_test_pred12,glass_test_labels)
     accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
     accuracy(tab)
     tab <- table(glass_test_pred13,glass_test_labels)
     accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
     accuracy(tab)
     #ACC.1 <- 100 * sum(glass_test_labels == glass_test_pred)/NROW(glass_test_labels)
       #ACC.1
       library(lattice)
     library(ggplot2)
     library(caret)
     confusionMatrix(table(glass_test_pred12 ,glass_test_labels))
     i=1
     k.optm=1
     for (i in 1:50){
        knn.mod <- knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=i)
          k.optm[i] <- 100 * sum(glass_test_labels == knn.mod)/NROW(glass_test_labels)
          k=i
         cat(k,'=',k.optm[i],'
                        ')
      }
     plot(k.optm, Type="b", xlab="K- Value",ylab="Accuracy level")
    
     # load the "gmodels" library
       library(gmodels)
     # Create the cross tabulation of predicted vs. actual
        CrossTable(x = glass_test_labels, y = glass_test_pred12,
                               prop.chisq=FALSE)
    
    
    
     # use the scale() function to z-score standardize a data frame
       glass_z <- as.data.frame(scale(glass[c(1:9)]))
       View(glass_z)
       # create training and test datasets
       dat.d <- sample(1:nrow(glass_z),size=nrow(glass_z)*0.7,replace = FALSE) #random selection of 70% data.
       
       glass_train <- glass_z[dat.d,] # 70% training data
       glass_test <- glass_z[-dat.d,] # remaining 30% test data
       #glass_train <- glass_n[1:150, ]
       #glass_test <- glass_n[151:214, ]
       View(glass_train)
       View(glass_test)
       glass_train_labels <- glass[dat.d,10]
       glass_test_labels <- glass[-dat.d,10]
       print(glass_train_labels)
       print(glass_test_labels)
       NROW(glass_train_labels)
       # re-classify test cases
           glass_test_pred12 <- knn(train = glass_train, test = glass_test,
                                                       cl = glass_train_labels, k=11)
           tab <- table(glass_test_pred12,glass_test_labels)
           accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
           accuracy(tab)
           glass_test_pred13 <- knn(train = glass_train, test = glass_test,
                                  cl = glass_train_labels, k=11)
           tab <- table(glass_test_pred13,glass_test_labels)
           accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
           accuracy(tab)
           
           #ACC.1 <- 100 * sum(glass_test_labels == glass_test_pred)/NROW(glass_test_labels)
             #ACC.1
             library(lattice)
           library(ggplot2)
           library(caret)
           confusionMatrix(table(glass_test_pred12 ,glass_test_labels))
           confusionMatrix(table(glass_test_pred13 ,glass_test_labels))
           i=1
           k.optm=1
           for (i in 1:50){
               glass_test_pred <- knn(train=glass_train, test=glass_test, cl=glass_train_labels, k=i)
               k.optm[i] <- 100 * sum(glass_test_labels == glass_test_pred)/NROW(glass_test_labels)
               k=i
               cat(k,'=',k.optm[i],'
                           ')
             }      
           # Create the cross tabulation of predicted vs. actual
             CrossTable(x = glass_test_labels, y = glass_test_pred12,
                                  prop.chisq=FALSE)
           plot(k.optm, Type="b", xlab="K- Value",ylab="Accuracy level")
        