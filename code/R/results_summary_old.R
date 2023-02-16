#load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_nnet_size5.rda")
#load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_nnet_size3.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg.rda")

#load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_nnet_size5.rda")
#load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_nnet_size3.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_overall.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal_overall.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal_overall.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_overall.rda")

load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_PC.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal_PC.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal_PC.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_PC.rda")

load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_rf_PC_overall.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_linear_kernal_PC_overall.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_svm_radial_kernal_PC_overall.rda")
load("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF/results/results_xg_PC_overall.rda")
 
res <- data.frame()
for (tooth in c("LM1","LM2","LM3","UM1","UM2","UM3")){
  #Class specific projection
  #acc = mean(results_nnet_size5[[tooth]][,1] == results_nnet_size5[[tooth]][,2])
  #res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "NNET5",proj = "IND",acc = acc))

  #acc = mean(results_nnet_size3[[tooth]][,1] == results_nnet_size3[[tooth]][,2])
  #res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "NNET3",proj = "IND",acc = acc))

  acc = mean(results_rf[[tooth]][,1] == results_rf[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3) ,method = "RF",proj = "I",acc = acc))
  
  acc = mean(results_svm_radial_kernal[[tooth]][,1] == results_svm_radial_kernal[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-RAD",proj = "I",acc = acc))
  
  acc = mean(results_svm_linear_kernal[[tooth]][,1] == results_svm_linear_kernal[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-LIN",proj = "I",acc = acc))
  
  acc = mean(results_xg[[tooth]][,1] == results_xg[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "XG",proj = "I",acc = acc))
  
  
#Overall   
  acc = mean(results_rf_overall[[tooth]][,1] == results_rf_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "RF",proj = "OV",acc = acc))
  
  acc = mean(results_svm_radial_kernal_overall[[tooth]][,1] == results_svm_radial_kernal_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-RAD",proj = "OV",acc = acc))
  
  acc = mean(results_svm_linear_kernal_overall[[tooth]][,1] == results_svm_linear_kernal_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-LIN",proj = "OV",acc = acc))
  
  acc = mean(results_xg_overall[[tooth]][,1] == results_xg_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "XG",proj = "OV",acc = acc))
  

  #IND-PC 
  acc = mean(results_rf_PC[[tooth]][,1] == results_rf_PC[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "RF",proj = "I-PC",acc = acc))
  
  acc = mean(results_svm_radial_kernal_PC[[tooth]][,1] == results_svm_radial_kernal_PC[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-RAD",proj = "I-PC",acc = acc))
  
  acc = mean(results_svm_linear_kernal_PC[[tooth]][,1] == results_svm_linear_kernal_PC[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-LIN",proj = "I-PC",acc = acc))
  
  acc = mean(results_xg_PC[[tooth]][,1] == results_xg_PC[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "XG",proj = "I-PC",acc = acc))
  
  #OVERALL-PC 
  acc = mean(results_rf_PC_overall[[tooth]][,1] == results_rf_PC_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "RF",proj = "OV-PC",acc = acc))
  
  acc = mean(results_svm_radial_kernal_PC_overall[[tooth]][,1] == results_svm_radial_kernal_PC_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-RAD",proj = "OV-PC",acc = acc))
  
  acc = mean(results_svm_linear_kernal_PC_overall[[tooth]][,1] == results_svm_linear_kernal_PC_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "SVM-LIN",proj = "OV-PC",acc = acc))
  
  acc = mean(results_xg_PC_overall[[tooth]][,1] == results_xg_PC_overall[[tooth]][,2])
  res <- rbind(res, data.frame(tooth = substring(tooth,1,2), toothnum = substring(tooth,3,3),method = "XG",proj = "OV-PC",acc = acc))
  
  }

res$proj <- factor(res$proj, levels = c("I","OV","I-PC","OV-PC"))

library(ggplot2)
ggplot(aes(x = proj, y = acc, colour = method, group = method), data = res) + geom_point(aes(group = method)) + facet_grid(toothnum~tooth) + 
  geom_line(aes(group = method))

