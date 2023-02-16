library(randomForest)
library(e1071)

results_rf <- list()
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
    
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    a <- randomForest(y = y_train, x = X_train)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, predict(a, X_test, "prob"))
    
  }
  
  results_rf[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_rf, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf.rda")


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


################################################################
#Random Forest overall 
################################################################
library(randomForest)
library(e1071)

results_rf_overall <- list()
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
    
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    a <- randomForest(y = y_train, x = X_train)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, predict(a, X_test, "prob"))
    
  }
  
  results_rf_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_rf_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_overall.rda")


for (i in 1:6){
  print(mean(results_rf_overall[[i]][,1] == results_rf_overall[[i]][,2]))
}

#Accuracy LM1: 0.7680412
#Accuracy LM2: 0.8573854
#Accuracy LM3: 0.7808696
#Accuracy UM1: 0.852349
#Accuracy UM2: 0.7967213
#Accuracy UM3: 0.7324185




################################################################
#Random Forest PC 
################################################################
library(randomForest)
library(e1071)

results_rf_PC <- list()
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
    
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    a <- randomForest(y = y_train, x = X_train)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, predict(a, X_test, "prob"))
    
  }
  
  results_rf_PC[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_rf_PC, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_PC.rda")


for (i in 1:6){
  print(mean(results_rf_PC[[i]][,1] == results_rf_PC[[i]][,2]))
}

#PC
#Accuracy LM1: 0.7886598
#Accuracy LM2: 0.8336163
#Accuracy LM3: 0.7565217
#Accuracy UM1: 0.8221477
#Accuracy UM2: 0.8032787
#Accuracy UM3: 0.7392796





################################################################
#Random Forest PC overall
################################################################
library(randomForest)
library(e1071)

results_rf_PC_overall <- list()
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
    
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    a <- randomForest(y = y_train, x = X_train)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, predict(a, X_test, "prob"))
    
  }
  
  results_rf_PC_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_rf_PC_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_PC_overall.rda")


for (i in 1:6){
  print(mean(results_rf_PC_overall[[i]][,1] == results_rf_PC_overall[[i]][,2]))
}

#PC overall
#Accuracy LM1: 0.7216495
#Accuracy LM2: 0.8047538
#Accuracy LM3: 0.7530435
#Accuracy UM1: 0.783557
#Accuracy UM2: 0.7606557
#Accuracy UM3:0.7084048

