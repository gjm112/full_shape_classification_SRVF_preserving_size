library(randomForest)
library(e1071)

results_svm_linear_kernal <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="linear", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "linear", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_linear_kernal[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_linear_kernal, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal.rda")

for (i in 1:6){
  print(mean(results_svm_linear_kernal[[i]][,1] == results_svm_linear_kernal[[i]][,2]))
}
#Accuracy LM1: 0.8367698
#Accuracy LM2: 0.8539898
#Accuracy LM3: 0.84
#Accuracy UM1: 0.885906
#Accuracy UM2: 0.852459
#Accuracy UM3: 0.780446





library(randomForest)
library(e1071)

results_svm_radial_kernal <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_radial_kernal[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_radial_kernal, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal.rda")

for (i in 1:6){
  print(mean(results_svm_radial_kernal[[i]][,1] == results_svm_radial_kernal[[i]][,2]))
}
#Radial Kernel
#Accuracy LM1: 0.8350515
#Accuracy LM2: 0.8709677
#Accuracy LM3: 0.8365217
#Accuracy UM1: 0.8674497
#Accuracy UM2: 0.8704918
#Accuracy UM3: 0.8336192

################################################################
#SVM linear kernel overall
################################################################
library(randomForest)
library(e1071)

results_svm_linear_kernal_overall <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="linear", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "linear", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_linear_kernal_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_linear_kernal_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal_overall.rda")

for (i in 1:6){
  print(mean(results_svm_linear_kernal_overall[[i]][,1] == results_svm_linear_kernal_overall[[i]][,2]))
}
#Accuracy LM1: 0.8006873
#Accuracy LM2: 0.8641766
#Accuracy LM3: 0.8052174
#Accuracy UM1: 0.8758389
#Accuracy UM2: 0.8377049
#Accuracy UM3: 0.7753002




################################################################
#SVM radial kernel overall
################################################################
library(randomForest)
library(e1071)

results_svm_radial_kernal_overall <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_radial_kernal_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_radial_kernal_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal_overall.rda")

for (i in 1:6){
  print(mean(results_svm_radial_kernal_overall[[i]][,1] == results_svm_radial_kernal_overall[[i]][,2]))
}
#Accuracy LM1: 0.8316151
#Accuracy LM2: 0.8777589
#Accuracy LM3: 0.8313043
#Accuracy UM1: 0.8791946
#Accuracy UM2: 0.8770492
#Accuracy UM3: 0.8113208




################################################################
#SVM linear kernel PC
################################################################
library(randomForest)
library(e1071)

results_svm_linear_kernal_PC <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="linear", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "linear", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_linear_kernal_PC[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_linear_kernal_PC, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal_PC.rda")

for (i in 1:6){
  print(mean(results_svm_linear_kernal_PC[[i]][,1] == results_svm_linear_kernal_PC[[i]][,2]))
}
#Accuracy LM1: 
#Accuracy LM2: 
#Accuracy LM3: 
#Accuracy UM1: 
#Accuracy UM2: 
#Accuracy UM3: 







################################################################
#SVM radial kernel PC
################################################################
library(randomForest)
library(e1071)

results_svm_radial_kernal_PC <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_radial_kernal_PC[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_radial_kernal_PC, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal_PC.rda")

for (i in 1:6){
  print(mean(results_svm_radial_kernal_PC[[i]][,1] == results_svm_radial_kernal_PC[[i]][,2]))
}
#Accuracy LM1: 0.8058419
#Accuracy LM2: 0.860781
#Accuracy LM3:  0.8
#Accuracy UM1:  0.8590604
#Accuracy UM2: 0.8491803
#Accuracy UM3:  0.7581475













################################################################
#SVM linear kernel PC overall
################################################################
library(randomForest)
library(e1071)

results_svm_linear_kernal_PC_overall <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="linear", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "linear", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_linear_kernal_PC_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_linear_kernal_PC_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal_PC_overall.rda")

for (i in 1:6){
  print(mean(results_svm_linear_kernal_PC_overall[[i]][,1] == results_svm_linear_kernal_PC_overall[[i]][,2]))
}
#Accuracy LM1: 
#Accuracy LM2: 
#Accuracy LM3: 
#Accuracy UM1: 
#Accuracy UM2: 
#Accuracy UM3: 







################################################################
#SVM radial kernel PC
################################################################
library(randomForest)
library(e1071)

results_svm_radial_kernal_PC_overall <- list()
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
    
    
    best <- tune(svm, train.y = y_train, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
    a <- svm(y = y_train, x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
    
    temp_list[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test, attr(predict(a, X_test, probability = TRUE), "probabilities"))
    
  }
  
  results_svm_radial_kernal_PC_overall[[toothtype]] <-  do.call(rbind,temp_list)
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_svm_radial_kernal_PC_overall, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal_PC_overall.rda")

for (i in 1:6){
  print(mean(results_svm_radial_kernal_PC_overall[[i]][,1] == results_svm_radial_kernal_PC_overall[[i]][,2]))
}
#Accuracy LM1: 0.8058419
#Accuracy LM2: 0.860781
#Accuracy LM3:  0.8
#Accuracy UM1:  0.8590604
#Accuracy UM2: 0.8491803
#Accuracy UM3:  0.7581475













