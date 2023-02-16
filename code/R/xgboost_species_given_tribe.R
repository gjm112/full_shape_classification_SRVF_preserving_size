start <- Sys.time()
library(xgboost)
library(caret)
library(e1071)

results_xg_tribe <- list()
results_xg_species <- list()
results_xg_species_given_tribe <- list()
#Projections: Individual, Overall, Individiual PC, Overall-PC

for (proj in c("I","OV","I-PC","OV-PC","EFA")){print(proj)
  results_xg_tribe[[proj]] <- list()
  results_xg_species[[proj]] <- list()
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
      
      lev_tribe <- levels(y_train$tribe)
      lev_species <- levels(y_train$species)
      
      X_train <- as.matrix(X_train)
      X_test <- as.matrix(X_test)
      
      #best <- tune(randomForest, train.y = y_train, train.x = X_train, ranges = list(mtry = c(3, 5), tunecontrol = tune.control(cross = 3)))
      #Make this the correct format
      ###################################
      #Tribe classification
      ###################################
      nrounds <- 500
      grid_default <- expand.grid(
        nrounds = seq(from = 50, to = nrounds, by = 50),
        eta = c(0,0.15,0.3,0.45),
        max_depth = c(2),
        gamma = c(0,0.15,0.3,0.45),
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      )

      train_control <- caret::trainControl(
        method = "cv", # cross-validation
        number = 3,
        verboseIter = TRUE, # no training log
        allowParallel = TRUE # FALSE for reproducible results
      )

      xgb_base <- caret::train(
        x = X_train,
        y = as.factor(as.numeric(as.factor(y_train$tribe)) - 1),
        trControl = train_control,
        tuneGrid = grid_default,
        method = "xgbTree",
        verbose = TRUE, 
        metric = "Accuracy", 
        maximize = TRUE
      )
      
      
      
      
     a <- xgboost(data = X_train, label = (as.numeric(as.factor(y_train$tribe)) - 1),
                   max_depth = xgb_base$bestTune$max_depth, 
                   eta = xgb_base$bestTune$eta, 
                   nthread = 2, 
                   gamma = xgb_base$bestTune$gamma,
                   nrounds = xgb_base$bestTune$nrounds,
                   num_class = 7,
                   objective = "multi:softprob", 
                   eval_metric = 'mlogloss'
                   )
      
      temp_list_tribe[[i]] <- data.frame(pred_class = lev_tribe[apply(predict(a, X_test, reshape = TRUE),1,which.max)],
                                         real_class = y_test$tribe, 
                                         predict(a, X_test, reshape = TRUE))
      names(temp_list_tribe[[i]])[3:9] <- lev_tribe
      
      
      print("a")
      print(xgb_base$bestTune)
      
      
      ###Species training
      nrounds <- 500
      grid_default <- expand.grid(
        nrounds = seq(from = 50, to = nrounds, by = 50),
        eta = c(0,0.15,0.3,0.45),
        max_depth = c(2),
        gamma = c(0,0.15,0.3,0.45),
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      )
      
      train_control <- caret::trainControl(
        method = "cv", # cross-validation
        number = 3,
        verboseIter = TRUE, # no training log
        allowParallel = TRUE # FALSE for reproducible results 
      )
      
      xgb_base <- caret::train(
        x = X_train,
        y = as.factor(as.numeric(as.factor(y_train$species)) - 1),
        trControl = train_control,
        tuneGrid = grid_default,
        method = "xgbTree",
        verbose = TRUE,
        metric = "Accuracy", 
        maximize = TRUE
      )
      
      print("b")
      print(xgb_base$bestTune)
      
      b <- xgboost(data = X_train, label = (as.numeric(as.factor(y_train$species)) - 1),
                   max_depth = xgb_base$bestTune$max_depth, 
                   eta = xgb_base$bestTune$eta, 
                   nthread = 2, 
                   gamma = xgb_base$bestTune$gamma,
                   nrounds = xgb_base$bestTune$nrounds,
                   num_class = 20,
                   objective = "multi:softprob", 
                   eval_metric = 'mlogloss'
      )
      temp_list_species[[i]] <- data.frame(pred_class = lev_species[apply(predict(b, X_test, reshape = TRUE),1,which.max)],
                                           real_class = y_test$species, 
                                           predict(b, X_test, reshape = TRUE))
      
      #Loop through tribes
      temp_list_species_given_tribe_c <- list()
      for (tr in c("Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini")){print(tr)
        if (!tr %in% c("Antilopini","Bovini")){
          num_class <- length(unique(as.numeric(as.factor(y_train$species[y_train$tribe == tr])) - 1))
          
          nrounds <- 500
          grid_default <- expand.grid(
            nrounds = seq(from = 50, to = nrounds, by = 50),
            eta = c(0,0.15,0.3,0.45),
            max_depth = c(2),
            gamma = c(0,0.15,0.3,0.45),
            colsample_bytree = 1,
            min_child_weight = 1,
            subsample = 1
          )
          
          train_control <- caret::trainControl(
            method = "cv", # cross-validation
            number = 3,
            verboseIter = TRUE, # no training log
            allowParallel = TRUE # FALSE for reproducible results 
          )
          
          xgb_base <- caret::train(
            x = X_train[y_train$tribe == tr,],
            y = as.factor((as.numeric(as.factor(as.character(y_train$species[y_train$tribe == tr])))) - 1),
            trControl = train_control,
            tuneGrid = grid_default,
            method = "xgbTree",
            verbose = TRUE,
            metric = "Accuracy", 
            maximize = TRUE
          )
          
          print("c")
          print(xgb_base$bestTune)
          
          c <- xgboost(data = X_train[y_train$tribe == tr,], label = ((as.numeric(as.factor(as.character(y_train$species[y_train$tribe == tr])))) - 1),
                       max_depth = xgb_base$bestTune$max_depth, 
                       eta = xgb_base$bestTune$eta, 
                       nthread = 2, 
                       gamma = xgb_base$bestTune$gamma,
                       nrounds = xgb_base$bestTune$nrounds,
                       num_class = num_class,
                       objective = "multi:softprob", 
                       eval_metric = 'mlogloss'
          )
         
          temp_list_species_given_tribe_c[[tr]] <- data.frame(apply(predict(c, X_test, reshape = TRUE),2,function(x){x*temp_list_tribe[[i]][,tr]}))
          names(temp_list_species_given_tribe_c[[tr]]) <- levels(as.factor(as.character(y_train$species[y_train$tribe == tr])))
          
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
    
    results_xg_tribe[[proj]][[toothtype]] <-  do.call(rbind,temp_list_tribe)
    results_xg_species[[proj]][[toothtype]] <-  do.call(rbind,temp_list_species)
    results_xg_species_given_tribe[[proj]][[toothtype]] <-  do.call(rbind,temp_list_species_given_tribe)
    
    #Output is a list with 6 slots (one for each tooth type).
    #In each slot there is the pred class, real class, and then probs for each tribe.
    save(results_xg_tribe, 
         file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_tribe.rda")
    
    save(results_xg_species, 
         file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_species.rda")
    
    save(results_xg_species_given_tribe, 
         file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_species_given_tribe.rda")
    
  }
  
}
end <- Sys.time()
end-start

#Output is a list with 6 slots (one for each tooth type).
#In each slot there is the pred class, real class, and then probs for each tribe.
save(results_xg_tribe, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_tribe.rda")

save(results_xg_species, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_species.rda")

save(results_xg_species_given_tribe, 
     file = "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_species_given_tribe.rda")


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


