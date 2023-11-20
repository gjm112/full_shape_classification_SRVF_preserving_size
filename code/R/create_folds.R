for (tooth in c("LM1", "LM2", "LM3", "UM1", "UM2", "UM3")) {print(tooth)

  
  ref <- read.csv(paste0("./data/fulldata/",tooth,"_train_reference.csv"))
  
  folds_ref <-
    read.csv(
      paste0(
        "/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/folds/",
        tooth,
        "ref_folds.csv"
      )
    )

  for (fold in 1:5) {print(fold)  
  
  file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/UpdatedCatsFiles/",tooth,"/",tooth,"fold_train_cats",fold,".csv")
  write.csv(ref[ref$image %in% folds_ref$image[folds_ref$folds_tribe != fold],c("tribe","species")],file = file)    
  file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/UpdatedCatsFiles/",tooth,"/",tooth,"fold_test_cats",fold,".csv")
  write.csv(ref[ref$image %in% folds_ref$image[folds_ref$folds_tribe == fold],c("tribe","species")],file = file)    

  
for (proj in c("I","OV","I-PC","OV-PC")){print(proj)
#Now cut it in to folds like Nikki did.
#Create the folds to match what nikki did.  Then I can just run it through the same code that I alreayd have.
  
  #Now pull out LM1 
  if (proj == "I"){temp <- read.csv(paste0("./data/fulldata/",tooth,"_train_individual.csv"),header = FALSE)}
  if (proj == "OV"){temp <- read.csv(paste0("./data/fulldata/",tooth,"_train_overall.csv"),header = FALSE)}
  if (proj == "I-PC"){temp <- read.csv(paste0("./data/fulldata/",tooth,"_train_individual_PC.csv"),header = FALSE)}
  if (proj == "OV-PC"){temp <- read.csv(paste0("./data/fulldata/",tooth,"_train_overall_PC.csv"),header = FALSE)}
  
  
  
    #Test
    if (proj == "I"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_test",fold,".csv")}
    if (proj == "OV"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_test_overall",fold,".csv")}
    if (proj == "I-PC"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_PCtest",fold,".csv")}
    if (proj == "OV-PC"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_PCtest_overall",fold,".csv")}
    
    write.csv(temp[ref$image %in% folds_ref$image[folds_ref$folds_tribe == fold],],file = file)
    
    #Train
    if (proj == "I"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_train",fold,".csv")}
    if (proj == "OV"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_train_overall",fold,".csv")}
    if (proj == "I-PC"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_PCtrain",fold,".csv")}
    if (proj == "OV-PC"){file = paste0("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/",tooth,"/",tooth,"fold_PCtrain_overall",fold,".csv")}
    
    write.csv(temp[ref$image %in% folds_ref$image[folds_ref$folds_tribe != fold],],file = file)    
    
  
    

    
  }
}

  
}


#Make a data frame.  
#Cut into folds.  
#Perform random forest and SVM to evaluate the methods on both tribe and species.  
#Evaluate accuracy 





