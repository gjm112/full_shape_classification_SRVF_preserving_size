library(vegan)
library(jpeg)
library(tidyverse)
library(dplyr)
library(Momocs)
library(fdasrvf)

source("/gladysvale/R/utility.R")
source("./gladysvale/R/curve_functions.R")

load("./gladysvale/RData/teeth_BW_train_20210622.RData")

make_same_num_points <- function(x, N = 500){
  out <- resamplecurve(t(x),N)
  return(out)
}

teeth_BW_train_500 <- lapply(teeth_BW_train, make_same_num_points)


#DSCN2871 and DSCN2875
plot(t(teeth_BW_train_500[[1]]))
points(t(teeth_BW_train_500[[1]])[175:176,],col = "red", pch = 16)
#I used 18 in the last one.  
plot(t(teeth_BW_train_500[[100]]))
points(t(teeth_BW_train_500[[100]])[170:171,],col = "red")

X <- teeth_BW_train_500[[1]]
Y <- teeth_BW_train_500[[100]]
X <- coo_slide(t(X), id = 175)
Y <- coo_slide(t(Y), id = 170)


test <- procrustes(X, Y, scale = FALSE)
# plot(test$X)
# plot(test$Yrot)

test$X[500,] <- test$X[1,]
test$Yrot[500,] <- test$Yrot[1,]

alpha <- 0
stack <- data.frame()
for (alpha in c(0, .25, .5, .75, 1)) {
  stack <- rbind(stack, alpha * test$X + (1 - alpha) * test$Yrot)
}

names(stack) <- c("x","y")
stack$alpha <- rep(c(0, .25, .5, .75, 1), each = 500)

png("./full_shape_classification_SRVF_preserving_size/deformations_EFA_for_paper.png", res = 300, h = 3, w = 15, units = "in")
ggplot(aes(x = x, y = y), data = stack) + 
  geom_path() + 
  facet_grid( ~ factor(alpha)) + 
  scale_x_continuous(breaks=c(-200,0,200)) + 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) 
  
dev.off()
  


#Now do the deformations for SRVF.  
#Covert to SRVF
#Align
#Compute deformations
#convert back to beta
qX <- curve_to_q(teeth_BW_train_500[[1]])
qY <- curve_to_q(teeth_BW_train_500[[100]])

write.csv(qX,file = "./full_shape_classification_SRVF_preserving_size/qX.csv",row.names = FALSE)
write.csv(qY,file = "./full_shape_classification_SRVF_preserving_size/qY.csv",row.names = FALSE)

#Register in matlab
#Find rotation and seed unique.m
#Run alignment_for_paper_deformation_plots.m
#Used to align the curves with reparameterization.  

qY_registered <- read.csv("./full_shape_classification_SRVF_preserving_size/qY_registered.csv", header = FALSE)

qX <- t(qX)
qY_registered <- t(qY_registered)

alpha <- 0
stack <- data.frame()
for (alpha in c(0, .25, .5, .75, 1)) {
  q <- alpha * qX + (1 - alpha) * qY_registered
  beta <- t(q_to_curve(t(q)))
  stack <- rbind(stack,beta)
}

names(stack) <- c("x","y")
stack$alpha <- rep(c(0, .25, .5, .75, 1), each = 500)

png("./full_shape_classification_SRVF_preserving_size/deformations_elastic_for_paper.png", res = 300, h = 3, w = 15, units = "in")
ggplot(aes(x = x, y = y), data = stack) + 
  geom_path() + 
  # geom_point(aes(x = x, y = y),data = stack[c(0,500,1000,1500,2000) + 50,
  # ], colour = "red") +
  # geom_point(aes(x = x, y = y),data = stack[c(0,500,1000,1500,2000) + 250,
  # ], colour = "blue") +
  # geom_point(aes(x = x, y = y),data = stack[c(0,500,1000,1500,2000) + 450,
  # ], colour = "yellow") +
  facet_grid( ~ factor(alpha)) + 
  scale_x_continuous(breaks=c(-200,0,200)) + 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) 

dev.off()
