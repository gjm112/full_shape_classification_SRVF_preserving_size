set.seed(1234)
library(randomForest)
library(e1071)

#Predict the gladysvale fossil teeth using individual class means.  
setwd("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/")
gladysvale_metadata <- read.csv("./data/Gladysvale_Med_Alcels_Greg.csv", header = FALSE)[,1:3]
names(gladysvale_metadata) <- c("image","type","broken")
gladysvale_metadata$I_broken <- 0
gladysvale_metadata$I_broken[gladysvale_metadata$broken == "broken"] <- 1
gladysvale_metadata <- gladysvale_metadata[gladysvale_metadata$broken != "broken",]
table(gladysvale_metadata$type)

for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){print(toothtype)
#Read in the training data set. 
X_train <- read.csv(paste0("./data/fulldata/",toothtype,"_train_individual.csv"), header = FALSE)
y_train <- read.csv(paste0("./data/fulldata/",toothtype,"_train_reference.csv"))

y_train$tribe <- as.factor(y_train$tribe)
y_train$species <- as.factor(y_train$species)

#Read in the test data set. 
X_test <- read.csv(paste0("./data/fulldata/",toothtype,"_gladysvale_individual.csv"), header = FALSE)
#This file has the reference numbers for the teeth in X_test
gladysvale_reference <- read.csv("./data/fulldata/gladysvale_reference.csv")

#remove the broken teeth
keep <- gladysvale_reference$teeth_ref_gladysvale %in% gladysvale_metadata$image
X_test <- X_test[keep,]
gladysvale_reference <- gladysvale_reference[keep,]

#rs in the full reference file.  Merge on the info that we want.  
rs <- read.csv("/Users/gregorymatthews/Dropbox/gladysvale/reference_file_20210622.csv")

#Train the model
best <- tune(svm, train.y = y_train$tribe, train.x = X_train ,kernel ="radial", ranges = list(cost=c(0.1,0.5,1,2.5,5,10,100,1000), gamma=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100,1000)), tunecontrol = tune.control(cross = 3))
a <- svm(y = factor(y_train$tribe), x = X_train, type = "C-classification", kernel = "radial", cost =  best$best.parameters, probability = TRUE)

#Predict Tribe Gladysvale
pred_tribe_gladysvale <- data.frame(ID = gladysvale_reference, 
                                    type = toothtype,
                                    pred_class = colnames(attr(predict(a, X_test, probability = TRUE), "probabilities"))[apply(attr(predict(a, X_test, probability = TRUE), "probabilities"),1,which.max)], real_class =NA, attr(predict(a, X_test, probability = TRUE), "probabilities"))


#Now train models to predict species conditional on tribe
temp_list_species_given_tribe_c <- list()
for (tr in c("Alcelaphini","Antilopini","Bovini","Hippotragini","Neotragini","Reduncini","Tragelaphini")){print(tr)
  if (!tr %in% c("Antilopini","Bovini")){
    best <- tune(svm, train.y = y_train$species[y_train$tribe == tr], train.x = X_train[y_train$tribe == tr,] ,kernel ="radial", ranges = list(cost=c(0.1,0.5,1,2.5,5,10,100,1000), gamma=c(0.001,0.01,0.1,0.5,1,2.5,5,10,100,1000)), tunecontrol = tune.control(cross = 3))
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

pred_tribe_gladysvale <- subset(pred_tribe_gladysvale,  ID %in% ids)
pred_species_gladysvale <- subset(pred_species_gladysvale,  ID %in% ids)


write.csv(pred_tribe_gladysvale, paste0("./gladysvale_predictions/",toothtype,"_tribe_individual.csv"))
write.csv(pred_species_gladysvale, paste0("./gladysvale_predictions/",toothtype,"_species_individual.csv"))


}

res <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
res[[toothtype]] <- read.csv(paste0("./gladysvale_predictions/",toothtype,"_tribe_individual.csv"))
}
res_df <- do.call(rbind,res)
write.csv(res_df,file = "./gladysvale_predictions/gladysvale_predictions_tribe_individual_master.csv")
#library(ggplot2)
#ggplot(aes(x = type, y = Alcelaphini), data = res_df) + geom_boxplot()

table(res_df$type,res_df$pred_class)
# table(res_df$pred_class)
# table(res_df$pred_class == "Alcelaphini")
# mean(res_df$pred_class == "Alcelaphini")
# 
# mean(res_df$Alcelaphini[res_df$pred_class == "Alcelaphini"])



#Species classification
res_spec <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res_spec[[toothtype]] <- read.csv(paste0("./gladysvale_predictions/",toothtype,"_species_individual.csv"))
}
res_df_spec <- do.call(rbind,res_spec)
write.csv(res_df_spec,file = "./gladysvale_predictions/gladysvale_predictions_species_individual_master.csv")

table(res_df_spec$pred_class) 
# mean(res_df$pred_class == "Alcelaphini")
# table(res_df$type,res_df$pred_class)

buselaphus, dorcas, gnou, taurinus
5, 6, 3, 1

cbind(res_df$pred_class,res_df_spec$pred_class)
