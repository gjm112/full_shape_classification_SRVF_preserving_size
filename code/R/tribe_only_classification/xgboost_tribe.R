library(xgboost)
library(e1071)

results_xg <- list()
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
    
    lev_tribe <- levels(y_train)
    
    y_train <- as.numeric(y_train) - 1
    y_test <- as.numeric(y_test) - 1
    
    X_train <- as.matrix(X_train)
    X_test <- as.matrix(X_test)
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    #a <- randomForest(y = y_train, x = X_train)
    a <- xgboost(data = X_train, label = y_train, 
                 max_depth = 2, 
                 eta = 1, 
                 nthread = 2, 
                 nrounds = 2,
                 num_class = 7,
                 objective = "multi:softprob", 
                 eval_metric = 'mlogloss')
                 
    
    predict(a, X_test, reshape = TRUE)
  
    temp_list[[i]] <- data.frame(pred_class = lev_tribe[predict(a, X_test)+1], real_class = y_test)
    
  }
  
  results_xg[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_xg, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg.rda")


for (i in 1:6){
  print(mean(results_rf[[i]][,1] == results_rf[[i]][,2]))
}

####################################################
#Overall 
####################################################
library(xgboost)
library(e1071)

results_xg_overall <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
  temp_list <- list()
  path <- "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/"
  for (i in 1:5){print(i)
    
    X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train_overall",i,".csv"), header = FALSE)
    
    X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_test_overall",i,".csv"), header = FALSE)
    
    y_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = FALSE)
    
    y_test <-  read.csv(paste0(path,toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = FALSE)
    
    y_train <- (as.factor(y_train$V1))
    y_test <- (as.factor(y_test$V1))
    
    lev_tribe <- levels(y_train)
    
    y_train <- as.numeric(y_train) - 1
    y_test <- as.numeric(y_test) - 1
    
    X_train <- as.matrix(X_train)
    X_test <- as.matrix(X_test)
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    #a <- randomForest(y = y_train, x = X_train)
    a <- xgboost(data = X_train, label = y_train, 
                 max_depth = 2, 
                 eta = 1, 
                 nthread = 2, 
                 nrounds = 2,
                 num_class = 7,
                 objective = "multi:softmax", 
                 eval_metric = 'mlogloss')
    
    
    temp_list[[i]] <- data.frame(pred_class = lev_tribe[predict(a, X_test)+1], real_class = y_test)
    
  }
  
  results_xg_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_xg_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_overall.rda")


for (i in 1:6){
  print(mean(results_rf[[i]][,1] == results_rf[[i]][,2]))
}

#Accuracy LM1: 0.8195876
#Accuracy LM2: 0.8505942
#Accuracy LM3: 0.7808696
#Accuracy UM1: 0.840604
#Accuracy UM2: 0.8147541
#Accuracy UM3: 0.728988

#overall
#Accuracy LM1: 0.7680412
#Accuracy LM2: 0.8573854
#Accuracy LM3: 0.7808696
#Accuracy UM1: 0.852349
#Accuracy UM2: 0.7967213
#Accuracy UM3: 0.7324185

####################################################
#PC
####################################################
library(xgboost)
library(e1071)

results_xg_PC <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
  temp_list <- list()
  path <- "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/"
  for (i in 1:5){print(i)
    
    X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtrain",i,".csv"), header = FALSE)
    
    X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtest",i,".csv"), header = FALSE)
    
    y_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = FALSE)
    
    y_test <-  read.csv(paste0(path,toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = FALSE)
    
    y_train <- (as.factor(y_train$V1))
    y_test <- (as.factor(y_test$V1))
    
    lev_tribe <- levels(y_train)
    
    y_train <- as.numeric(y_train) - 1
    y_test <- as.numeric(y_test) - 1
    
    X_train <- as.matrix(X_train)
    X_test <- as.matrix(X_test)
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    #a <- randomForest(y = y_train, x = X_train)
    a <- xgboost(data = X_train, label = y_train, 
                 max_depth = 2, 
                 eta = 1, 
                 nthread = 2, 
                 nrounds = 2,
                 num_class = 7,
                 objective = "multi:softmax", 
                 eval_metric = 'mlogloss')
    
    
    temp_list[[i]] <- data.frame(pred_class = lev_tribe[predict(a, X_test)+1], real_class = y_test)
    
  }
  
  results_xg_PC[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_xg_PC, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_PC.rda")

####################################################
#PC over all
####################################################
library(xgboost)
library(e1071)

results_xg_PC_overall <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
  temp_list <- list()
  path <- "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/"
  for (i in 1:5){print(i)
    
    X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtrain_overall",i,".csv"), header = FALSE)
    
    X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtest_overall",i,".csv"), header = FALSE)
    
    y_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = FALSE)
    
    y_test <-  read.csv(paste0(path,toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = FALSE)
    
    y_train <- (as.factor(y_train$V1))
    y_test <- (as.factor(y_test$V1))
    
    lev_tribe <- levels(y_train)
    
    y_train <- as.numeric(y_train) - 1
    y_test <- as.numeric(y_test) - 1
    
    X_train <- as.matrix(X_train)
    X_test <- as.matrix(X_test)
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    #a <- randomForest(y = y_train, x = X_train)
    a <- xgboost(data = X_train, label = y_train, 
                 max_depth = 2, 
                 eta = 1, 
                 nthread = 2, 
                 nrounds = 2,
                 num_class = 7,
                 objective = "multi:softmax", 
                 eval_metric = 'mlogloss')
    
    
    temp_list[[i]] <- data.frame(pred_class = lev_tribe[predict(a, X_test)+1], real_class = y_test)
    
  }
  
  results_xg_PC_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_xg_PC_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_PC_overall.rda")
