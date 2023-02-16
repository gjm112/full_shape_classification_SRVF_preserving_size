#Classification of complete shapes using the SRVF framework. 
#We are going to compare the cross validation error rates using the SRVF 
#to the error rates that we found using the elliptical fourier analysis framework.


#Load packages
library(randomForest)
library(tidyverse)
library(tune)


toothtype <- "LM1"
i <- 1

#Read in the data
results <- list()
for (i in 1:5){print(i)
X_train <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_train",i,".csv"), header = FALSE)
X_test <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_test",i,".csv"), header = FALSE)

y_train <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = FALSE)
y_test <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/",toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = FALSE)

y_train <- (as.factor(y_train$V1))
y_test <- (as.factor(y_test$V1))

#tune the random forest
fold <- sample(1:5, nrow(X_train), replace = TRUE)
acc <- c()
mtry_vec <- seq(10,100,10)
for (m in 1:length(mtry_vec)){print(m)
tune_list <- list()
for (j in 1:5){print(j)
tune_forest <- randomForest(X_train[fold != j,], y_train[fold != j], mtry = mtry_vec[m])
tune_list[[j]] <- data.frame(pred_class = predict(tune_forest, X_train[fold == j,]), real_class = y_train[fold == j])
}  

tune_preds <- do.call(rbind, tune_list)
acc[m] <- mean(tune_preds[,1] == tune_preds[,2])
}
  
mtry <- mtry_vec[which.max(acc)]
rf <- randomForest(X_train, y_train, mtry = mtry)


results[[i]] <- data.frame(pred_class = predict(rf, X_test), real_class = y_test)

}

res <- do.call(rbind,results)
mean(res[,1] == res[,2])


(table(results[[1]]))

