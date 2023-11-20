PC1 <- read.csv("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/PC1_for_plot.csv", header = FALSE)
PC2 <- read.csv("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/PC2_for_plot.csv", header = FALSE)
PC3 <- read.csv("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/data/PC3_for_plot.csv", header = FALSE)

dat <- data.frame(rbind(PC1,PC2,PC3))
#Rotate 
theta <- 4.1
rot <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow = 2)
dat <- as.data.frame(as.matrix(dat) %*% rot)
dat$k <- rep(rep(-3:3,each = 100),3)
dat$k <- rep(rep(seq(-1.5,1.5,0.5),each = 100),3)
dat$PC <- rep(c("PC1","PC2","PC3"), each = 700)

names(dat)[1:2] <- c("x","y")

png("/Users/gregorymatthews/Dropbox/full_shape_classification_SRVF_preserving_size/PCA_plot_for_paper.png", res = 300, h = 3, w = 7, units = "in")
ggplot(aes(x = x, y = y), data = dat) + geom_path() + facet_grid(factor(PC)~factor(k)) + theme(aspect.ratio =1) + scale_x_continuous(breaks=c(-200,0,200)) + scale_y_continuous(breaks=c(-200,0,200))  + geom_path(aes(x = x, y= y), col = "red", data = dat %>% filter(k == 0)) 
dev.off()