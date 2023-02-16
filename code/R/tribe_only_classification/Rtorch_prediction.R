X_train
y_train$tribe

  X_train <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtrain_overall",i,".csv"), header = FALSE)
  X_test <- read.csv(paste0(path,toothtype,"/",toothtype,"fold_PCtest_overall",i,".csv"), header = FALSE)


y_train <- read.csv(paste0(path,"UpdatedCatsFiles/",toothtype,"/",toothtype,"fold_train_cats",i,".csv"), header = TRUE)
y_test <-  read.csv(paste0(path,"UpdatedCatsFiles/",toothtype,"/",toothtype,"fold_test_cats",i,".csv"), header = TRUE)


y_train$tribe <- (as.factor(y_train$tribe))
y_test$tribe <- (as.factor(y_test$tribe))

y_train$species <- (as.factor(y_train$species))
y_test$species <- (as.factor(y_test$species))

# 2. Convert our input data to matrices and labels to vectors.
X_train = as.matrix(X_train)
y_train = as.numeric(y_train$tribe) 
X_test = as.matrix(X_test)
y_test = as.numeric(y_test$tribe) 

X_train <- torch_tensor(X_train)
y_train <- torch_tensor(y_train, dtype = torch_long())
X_test <- torch_tensor(X_test)
y_test <- torch_tensor(y_test, dtype = torch_long())

model = nn_sequential(
  
  # Layer 1
  nn_linear(ncol(X_train), 64),
  nn_relu(), 
  
  # Layer 2
  nn_linear(64, 32),
  nn_relu(),
  
  # Layer 3
  nn_linear(32, 16),
  nn_relu(),
  
  # Layer 4
  nn_linear(16, 7),
  nn_softmax(2)
  
)

# Define cost and optimizer
criterion = nn_cross_entropy_loss()  
optimizer = optim_adam(model$parameters, lr = 0.01)

epochs = 2000

# Train the net
for(i in 1:epochs){
  
  optimizer$zero_grad()
  
  y_pred = model(X_train)
  loss = criterion(y_pred, y_train)
  loss$backward()
  optimizer$step()
  
  
  # Check Training
  if(i %% 10 == 0){
    
    winners = y_pred$argmax(dim=2)
    corrects = (winners == y_train)
    accuracy = corrects$sum()$item() / y_train$size()
    
    cat(" Epoch:", i,"Loss: ", loss$item()," Accuracy:",accuracy,"\n")
  }
  
}

y_pred = model(X_test)
as.matrix(y_pred)

y_pred$argmax(dim = 2)



