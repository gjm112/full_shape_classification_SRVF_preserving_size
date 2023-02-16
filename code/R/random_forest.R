


library(randomForest)
toothtype <- "LM1"
i <- 1
results <- list()

for (i in 1:5){print(i)
X_train <- read.csv(paste0("/Users/gregorymatthews/Downloads/Five_Fold_Classification/",toothtype,"/",toothtype,"fold_train",i,".csv"), header = FALSE)

X_test <- read.csv(paste0("/Users/gregorymatthews/Downloads/Five_Fold_Classification/",toothtype,"/",toothtype,"fold_test",i,".csv"), header = FALSE)

y_train <- read.csv(paste0("/Users/gregorymatthews/Downloads/Five_Fold_Classification/",toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = FALSE)

y_test <-  read.csv(paste0("/Users/gregorymatthews/Downloads/Five_Fold_Classification/",toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = FALSE)

y_train <- (as.factor(y_train$V1))
y_test <- (as.factor(y_test$V1))


a <- randomForest(X_train, y_train)
results[[i]] <- data.frame(pred_class = predict(a, X_test), real_class = y_test)

}

res <- do.call(rbind,results)


mean(res[,1] == res[,2])

table(res[,1], res[,2])



