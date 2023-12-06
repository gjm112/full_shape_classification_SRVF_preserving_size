set.seed(1234)
library(randomForest)
library(e1071)

#Predict the gladysvale fossil teeth using overall means  
gladysvale_metadata <- read.csv("./data/Gladysvale_Med_Alcels_Greg.csv", header = FALSE)[,1:3]
names(gladysvale_metadata) <- c("image","type","broken")
gladysvale_metadata$I_broken <- 0
gladysvale_metadata$I_broken[gladysvale_metadata$broken == "broken"] <- 1
gladysvale_metadata <- gladysvale_metadata[gladysvale_metadata$broken != "broken",]
table(gladysvale_metadata$type)

for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  #Read in the training data set. 
  X_train <- read.csv(paste0("./data/fulldata/",toothtype,"_train_overall.csv"), header = FALSE)
  y_train <- read.csv(paste0("./data/fulldata/",toothtype,"_train_reference.csv"))
  
  y_train$tribe <- as.factor(y_train$tribe)
  y_train$species <- as.factor(y_train$species)
  
  #Read in the test data set. 
  X_test <- read.csv(paste0("./data/fulldata/",toothtype,"_gladysvale_overall.csv"), header = FALSE)
  gladysvale_reference <- read.csv("./data/fulldata/gladysvale_reference.csv")
  
  #rs in the full reference file.  Merge on the info that we want.  
  rs <- read.csv("./gladysvale/reference_file_20210622.csv")
  
  #Train the model
  best <- tune(svm, train.y = y_train$tribe, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
  a <- svm(y = factor(y_train$tribe), x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
  
  #Predict Tribe Gladysvale
  pred_tribe_gladysvale <- data.frame(ID = gladysvale_reference, 
                                      type = toothtype,
                                      pred_class = colnames(attr(predict(a, X_test, probability = TRUE), "probabilities"))[apply(attr(predict(a, X_test, probability = TRUE), "probabilities"),1,which.max)], real_class =NA, attr(predict(a, X_test, probability = TRUE), "probabilities"))
  
  
  #Now train models to predict species conditional on tribe
  temp_list_species_given_tribe_c <- list()
  for (tr in c("Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini")){print(tr)
    if (!tr %in% c("Antilopini","Bovini")){
      best <- tune(svm, train.y = y_train$species[y_train$tribe == tr], train.x = X_train[y_train$tribe == tr,] ,kernel ="radial", ranges = list(cost=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100)), tunecontrol = tune.control(cross = 3))
      c <- svm(y = y_train$species[y_train$tribe == tr], x = X_train[y_train$tribe == tr,], type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)
      tribe_prob <- data.frame(attr(predict(a, X_test, probability = TRUE), "probabilities"))
      temp_list_species_given_tribe_c[[tr]] <- data.frame(apply(attr(predict(c, X_test, probability = TRUE), "probabilities"),2,function(x){x*tribe_prob[[tr]]}))
      
    }
    
    if (tr %in% c("Antilopini")){
      temp_list_species_given_tribe_c[[tr]] <- data.frame(marsupialis = pred_tribe_gladysvale$Antilopini)
    }
    
    if (tr %in% c("Bovini")){
      temp_list_species_given_tribe_c[[tr]] <- data.frame(caffer = pred_tribe_gladysvale$Bovini)
    }
  }  
  names(temp_list_species_given_tribe_c) <- NULL
  temp <- do.call(cbind,temp_list_species_given_tribe_c)
  
  pred_species_gladysvale <- data.frame(ID = gladysvale_reference, type = toothtype,
                                        pred_class = names(temp)[apply(temp,1,which.max)],
                                        real_class = NA, 
                                        temp)
  
  #Now pull out only the correct tooth type
  ids <- gladysvale_metadata$image[gladysvale_metadata$type == toothtype]
  
  pred_tribe_gladysvale <- subset(pred_tribe_gladysvale,  teeth_ref_gladysvale %in% ids)
  pred_species_gladysvale <- subset(pred_species_gladysvale,  teeth_ref_gladysvale %in% ids)
  

  write.csv(pred_tribe_gladysvale, paste0("./gladysvale_predictions/",toothtype,"_tribe_overall.csv"))
  write.csv(pred_species_gladysvale, paste0("./gladysvale_predictions/",toothtype,"_species_overall.csv"))
  
  
}



res <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res[[toothtype]] <- read.csv(paste0("./gladysvale_predictions/",toothtype,"_tribe_overall.csv"))
}
res_df <- do.call(rbind,res)
write.csv(res_df,file = "./gladysvale_predictions/gladysvale_predictions_tribe_overall_master.csv")


#Species classification
res_spec <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res_spec[[toothtype]] <- read.csv(paste0("./gladysvale_predictions/",toothtype,"_species_overall.csv"))
}
res_df_spec <- do.call(rbind,res_spec)
write.csv(res_df_spec,file = "./gladysvale_predictions/gladysvale_predictions_species_overall_master.csv")



