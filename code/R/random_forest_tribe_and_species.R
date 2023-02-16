library(randomForest)
library(e1071)

results_rf_tribe <- list()
results_rf_species <- list()
#Projections: Individual, Overall, Individiual PC, Overall-PC

for (proj in c("I","OV","I-PC","OV-PC","EFA")){print(proj)
#for (proj in c("EFA")){print(proj)
  results_rf_tribe[[proj]] <- list()
  results_rf_species[[proj]] <- list()
  
  for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
  temp_list_tribe <- list()
  temp_list_species <- list()
  path <- "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/"
    for (i in 1:5){print(i)
      
      if (proj == "EFA"){
        X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_EFAtrain",i,".csv"), header = TRUE)
        X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_EFAtest",i,".csv"), header = TRUE)
      }
    
    if (proj == "I"){
    X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train",i,".csv"), header = FALSE)
    X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_test",i,".csv"), header = FALSE)
    }  
    
    if (proj == "OV"){
    X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_train_overall",i,".csv"), header = FALSE)
    X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_test_overall",i,".csv"), header = FALSE)
    }
      
      if (proj == "I-PC"){
        X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtrain",i,".csv"), header = FALSE)
        X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtest",i,".csv"), header = FALSE)
      }
      
      if (proj == "OV-PC"){
        X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtrain_overall",i,".csv"), header = FALSE)
        X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtest_overall",i,".csv"), header = FALSE)
      }
      
  y_train <- read.csv(paste0(path,"UpdatedCatsFiles/",toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = TRUE)
  y_test <-  read.csv(paste0(path,"UpdatedCatsFiles/",toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = TRUE)
      
    
    y_train$tribe <- (as.factor(y_train$tribe))
    y_test$tribe <- (as.factor(y_test$tribe))
    
    y_train$species <- (as.factor(y_train$species))
    y_test$species <- (as.factor(y_test$species))
    
    
    #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
    #Make this the correct format
    ###################################
    #Tribe classification
    ###################################
    a <- randomForest(y = y_train$tribe, x = X_train)
    temp_list_tribe[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test$tribe, predict(a, X_test, "prob"))

    b <- randomForest(y = y_train$species, x = X_train)
    temp_list_species[[i]] <- data.frame(pred_class = predict(b, X_test), real_class = y_test$species, predict(b, X_test, "prob"))
    
    }
  
      results_rf_tribe[[proj]][[toothtype]] <-  do.call(rbind,temp_list_tribe)
      results_rf_species[[proj]][[toothtype]] <-  do.call(rbind,temp_list_species)
  
  }
  
}

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_rf_tribe, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_tribe.rda")

save(results_rf_species, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_species.rda")


for (i in 1:6){
  print(mean(results_rf_tribe[["I"]][[i]][,1] == results_rf_tribe[["I"]][[i]][,2]))
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


