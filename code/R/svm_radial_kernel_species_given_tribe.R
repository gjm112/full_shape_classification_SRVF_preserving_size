start <- Sys.time()
library(randomForest)
library(e1071)

results_svm_radial_tribe <- list()
results_svm_radial_species <- list()
results_svm_radial_species_given_tribe <- list()
#Projections: Individual, Overall, Individiual PC, Overall-PC

size <- FALSE

for (proj in c("I","OV","I-PC","OV-PC","EFA")){print(proj)
  results_svm_radial_tribe[[proj]] <- list()
  results_svm_radial_species[[proj]] <- list()
  temp_list_species_given_tribe <- list()
  
  for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
    temp_list_tribe <- list()
    temp_list_species <- list()
    path <- "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/"
    for (i in 1:5){print(i)
      
      if (proj == "EFA"){
        X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_EFAtrain",i,".csv"), header = TRUE)[,-1]
        X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_EFAtest",i,".csv"), header = TRUE)[,-1]
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
      
      if (size){
        fold_ref_with_size <- read.csv(paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/data/folds/",toothtype,"ref_folds_with_size.csv"))
        X_train$size <- fold_ref_with_size$size[fold_ref_with_size$folds_tribe != i]
        X_test$size <- fold_ref_with_size$size[fold_ref_with_size$folds_tribe == i]
      }
      
      #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
      #Make this the correct format
      ###################################
      #Tribe classification
      ###################################
      best <- tune(svm, train.y = y_train$tribe, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.1,0.5,1,2.5,5,10,100,1000), gamma=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100,1000)), tunecontrol = tune.control(cross = 3))
      a <- svm(y = y_train$tribe, x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
      temp_list_tribe[[i]] <- data.frame(pred_class = colnames(attr(predict(a, X_test, probability = TRUE), "probabilities"))[apply(attr(predict(a, X_test, probability = TRUE), "probabilities"),1,which.max)]
                                           , real_class = y_test$tribe, attr(predict(a, X_test, probability = TRUE), "probabilities"))
      #predict(a, X_test)  I'm not using this anymore because I want the predictions to line up with the probabilities.  
      
      best <- tune(svm, train.y = y_train$species, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.1,0.5,1,2.5,5,10,100,1000), gamma=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100,1000)), tunecontrol = tune.control(cross = 3))
      b <- svm(y = y_train$species, x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
      temp_list_species[[i]] <- data.frame(pred_class = colnames(attr(predict(b, X_test, probability = TRUE), "probabilities"))[apply(attr(predict(b, X_test, probability = TRUE), "probabilities"),1,which.max)], real_class = y_test$species, attr(predict(b, X_test, probability = TRUE), "probabilities"))
      
      #Loop through tribes
      temp_list_species_given_tribe_c <- list()
      for (tr in c("Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini")){print(tr)
        if (!tr %in% c("Antilopini","Bovini")){
          best <- tune(svm, train.y = y_train$species[y_train$tribe == tr], train.x = X_train[y_train$tribe == tr,] ,kernel ="radial", ranges = list(cost=c(0.1,0.5,1,2.5,5,10,100,1000), gamma=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100,1000)), tunecontrol = tune.control(cross = 3))
          c <- svm(y = y_train$species[y_train$tribe == tr], x = X_train[y_train$tribe == tr,], type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
          temp_list_species_given_tribe_c[[tr]] <- data.frame(apply(attr(predict(c, X_test, probability = TRUE), "probabilities"),2,function(x){x*temp_list_tribe[[i]][,tr]}))
          
        }
        
        if (tr %in% c("Antilopini")){
          temp_list_species_given_tribe_c[[tr]] <- data.frame(marsupialis = temp_list_tribe[[i]][[tr]])
        }
        
        if (tr %in% c("Bovini")){
          temp_list_species_given_tribe_c[[tr]] <- data.frame(caffer = temp_list_tribe[[i]][[tr]])
        }
      }  
      names(temp_list_species_given_tribe_c) <- NULL
      temp <- do.call(cbind,temp_list_species_given_tribe_c)
      
      temp_list_species_given_tribe[[i]] <- data.frame(pred_class = names(temp)[apply(temp,1,which.max)],
                                                       real_class = y_test$species, 
                                                       temp)
      
    }
    
    results_svm_radial_tribe[[proj]][[toothtype]] <-  do.call(rbind,temp_list_tribe)
    results_svm_radial_species[[proj]][[toothtype]] <-  do.call(rbind,temp_list_species)
    results_svm_radial_species_given_tribe[[proj]][[toothtype]] <-  do.call(rbind,temp_list_species_given_tribe)
    
  }
  
}
end <- Sys.time()
end-start

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
if (!size){
save(results_svm_radial_tribe, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_tribe.rda")

save(results_svm_radial_species, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_species.rda")

save(results_svm_radial_species_given_tribe, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_species_given_tribe.rda")
}

if (size){
  save(results_svm_radial_tribe, 
       file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_tribe_with_size.rda")
  
  save(results_svm_radial_species, 
       file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_species_with_size.rda")
  
  save(results_svm_radial_species_given_tribe, 
       file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_species_given_tribe_with_size.rda")
}

for (i in 1:6){
  print(mean(results_svm_radial_species_given_tribe[[i]][,1] == results_svm_radial_species_given_tribe[[i]][,2]))
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


