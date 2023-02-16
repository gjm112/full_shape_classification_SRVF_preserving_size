library(nnet)
library(e1071)
library(caret)
library(MLmetrics)

results_nnet_size5 <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
  temp_list <- list()
  path <- "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/"
  for (i in 1:5){print(i)
    
    X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train",i,".csv"), header = FALSE)
    
    X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_test",i,".csv"), header = FALSE)
    
    y_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = FALSE)
    
    y_test <-  read.csv(paste0(path,toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = FALSE)
    
    y_train <- (as.factor(y_train$V1))
    y_test <- (as.factor(y_test$V1))
    
    y_train_ind <- class.ind(y_train)
    y_test_ind <- class.ind(y_test)
    
    #best <- tune(nnet, train.y = y_train, train.x = X_train, MaxNWts = 10000,ranges = list(size = 1),tunecontrol = tune.control(cross = 3))
    a <- nnet(x = X_train,y = y_train_ind, size = 5, decay = 0.5, MaxNWts = 10000, maxit = 10000)
  
    pred_class <- attr(predict(a, X_test)[1,], "names")[apply(predict(a, X_test),1,which.max)]
    
    temp_list[[i]] <- data.frame(pred_class = pred_class, real_class = y_test, predict(a, X_test))

  }
  
  results_nnet_size5[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_nnet_size5, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_nnet_size5.rda")

for (i in 1:6){
  print(mean(results_nnet_size5[[i]][,1] == results_nnet_size5[[i]][,2]))
}

#Size 5.  Decay 0.5
#Accuracy LM1: 0.8092784 
#Accuracy LM2: 0.811545
#Accuracy LM3: 0.7947826
#Accuracy UM1: 0.8624161
#Accuracy UM2:  0.8442623
#Accuracy UM3: 0.7512864


#Size 3.  Decay 0.5
#Accuracy LM1:  0.7319588
#Accuracy LM2: 0.7572156
#Accuracy LM3: 0.7234783
#Accuracy UM1: 0.8053691
#Accuracy UM2: 0.7803279
#Accuracy UM3: 0.7186964

#Size 1.  Decay 0.5
#Accuracy LM1:  0.4329897
#Accuracy LM2: 0.4448217
#Accuracy LM3: 0.4330435
#Accuracy UM1: 0.4110738
#Accuracy UM2: 0.4065574
#Accuracy UM3: 0.4219554





