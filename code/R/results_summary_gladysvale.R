#Individual results
res <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res[[toothtype]] <- read.csv(paste0("./gladysvale_predictions/",toothtype,"_tribe_individual.csv"))
}


res_df <- do.call(rbind,res)
res_df$tooth_loc <- substring(res_df$type,1,2)
res_df$tooth_loc_num <- substring(res_df$type,3,3)
library(xtable)
xtable(table(res_df$tooth_loc_num,res_df$tooth_loc))

library(ggplot2)
ggplot(aes(x = type, y = Alcelaphini), data = res_df) + geom_boxplot()


table(res_df$type,res_df$pred_class)
table(res_df$pred_class == "Alcelaphini")
mean(res_df$pred_class == "Alcelaphini")


#Overall results
res <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res[[toothtype]] <- read.csv(paste0("./gladysvale_predictions/",toothtype,"_tribe_overall.csv"))
}


res_df <- do.call(rbind,res)
res_df$tooth_loc <- substring(res_df$type,1,2)
res_df$tooth_loc_num <- substring(res_df$type,3,3)
library(xtable)
xtable(table(res_df$tooth_loc_num,res_df$tooth_loc))

library(ggplot2)
ggplot(aes(x = type, y = Alcelaphini), data = res_df) + geom_boxplot()


table(res_df$type,res_df$pred_class)
table(res_df$pred_class == "Alcelaphini")
mean(res_df$pred_class == "Alcelaphini")



#Individual results species
res <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res[[toothtype]] <- read.csv (paste0("./gladysvale_predictions/",toothtype,"_species_individual.csv"))
}

res_df <- do.call(rbind,res)
res_df$tooth_loc <- substring(res_df$type,1,2)
res_df$tooth_loc_num <- substring(res_df$type,3,3)

#Species table individual predictions
#Tribe and species structure
tribe_and_species <- read.csv("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/UpdatedCatsFiles/LM1/LM1fold_test_cats1.csv")
tribe_and_species <- tribe_and_species[!duplicated(paste0(tribe_and_species$tribe, tribe_and_species$species)),]
#Individual results species
res <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res[[toothtype]] <- read.csv (paste0("./gladysvale_predictions/",toothtype,"_species_individual.csv"))
}

res_df <- do.call(rbind,res)
res_df$tooth_loc <- substring(res_df$type,1,2)
res_df$tooth_loc_num <- substring(res_df$type,3,3)

res_df <- merge(res_df, tribe_and_species, by.x = "pred_class", by.y = "species", all.x = TRUE)

xtable(table(res_df$type,paste0(res_df$tribe,res_df$pred_class)))
table(res_df$type,paste0(res_df$tribe,res_df$pred_class))



#Overeall results 
tribe_and_species <- read.csv("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/UpdatedCatsFiles/LM1/LM1fold_test_cats1.csv")
tribe_and_species <- tribe_and_species[!duplicated(paste0(tribe_and_species$tribe, tribe_and_species$species)),]
#Individual results species
res <- list()
for (toothtype in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  res[[toothtype]] <- read.csv (paste0("./gladysvale_predictions/",toothtype,"_species_overall.csv"))
}

res_df <- do.call(rbind,res)
res_df$tooth_loc <- substring(res_df$type,1,2)
res_df$tooth_loc_num <- substring(res_df$type,3,3)

res_df <- merge(res_df, tribe_and_species, by.x = "pred_class", by.y = "species", all.x = TRUE)


#xtable(table(res_df$type,res_df$pred_class))
xtable(table(res_df$type,paste0(res_df$tribe,res_df$pred_class)))
table(res_df$type,paste0(res_df$tribe,res_df$pred_class))




#buselaphus, taurinus, gnou, dorcas
sum(res_df$pred_class == "gnou")
sum(res_df$pred_class == "buselaphus")
sum(res_df$pred_class == "dorcas")
mean(res_df$pred_class == "taurinus")
