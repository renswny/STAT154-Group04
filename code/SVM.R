############################################################
# SVM
############################################################

# Load necessary libraries
library(e1071)
library(parallel)

# Setwd to root directory of project before reading data
setwd("~/Fall 2016/STAT 154/Final Project")

# Read training data
#train_df <- read.csv("STAT154-Group04/data/processed_train_df_4.csv")
#train_df <- train_df[,-1]
#train_df[,1] <- as.factor(train_df[,1])

# First column is target variable
# Second is number of words 
# Remaining are word frequencies

# Give variables names that are specific to model (SVM_mse instead of mse)

############################
######## FUNCTION ##########
############################

sum_lin <- function(i){
  tune.out <- tune(svm, emails.V1~ ., data = train_df, kernel = "linear", cost = cost_vec[i],
                   tunecontrol = tune.control(cross = 5))
  sum_tune.out <- summary(tune.out)
  svm_error <- sum_tune.out$best.performance
  names(svm_error) <- paste0("cost=", cost_vec[i])
  return(svm_error)
}

sum_poly <- function(i){
  tune.out <- tune(svm, emails.V1~ ., data = train_df, kernel = "polynomial", cost = cost_vec[i], degree = 2,
                   tunecontrol = tune.control(cross = 5))
  sum_tune.out <- summary(tune.out)
  svm_error <- sum_tune.out$best.performance
  names(svm_error) <- paste0("cost=", cost_vec[i])
  return(svm_error)
}

sum_rad <- function(i){
  tune.out <- tune(svm, emails.V1~ ., data = train_df, kernel = "radial",
                   cost = cost_gamma[[i]][1], gamma = cost_gamma[[i]][2],
                   tunecontrol = tune.control(cross = 5))
  sum_tune.out <- summary(tune.out)
  svm_error <- sum_tune.out$best.performance
  names(svm_error) <- paste("cost=", cost_gamma[[i]][1],"and","gamma=", cost_gamma[[i]][2], sep = " ")
  return(svm_error)
}

rem_columns <- function(data_set, col_start, cond_num){
  columns_rem <- c(1:(col_start-1))
  
  for (i in col_start:ncol(data_set)){
    if (sum(data_set[,i] > 0) > cond_num){
      columns_rem[length(columns_rem) + 1] <- i
    }
  }
  
  data_set <- data_set[,columns_rem]
  
  return(data_set)
}

############################
##### GLOBAL VARIABLES #####
############################

cost_vec <- c(.1, .5, 1, 10, 100)
gamma_vec <- c(.1, .5, 1, 5)
cost_gamma <- list()

for(i in 1:length(cost_vec)){
  for(j in 1:length(gamma_vec)){
    cost_gamma[[length(cost_gamma)+1]] <- c(cost_vec[i], gamma_vec[j])
  }
}

#######################
######## svm ##########
#######################

train_df <- read.csv("STAT154-Group04/data/processed_train_df_2.csv")
train_df <- train_df[,-1]
train_df[,1] <- as.factor(train_df[,1])

train_df <- rem_columns(train_df, 4, 9)

cl <- makeCluster(3)
clusterExport(cl, "train_df")
clusterExport(cl, "cost_vec")
clusterExport(cl, "cost_gamma")
clusterEvalQ(cl, library(e1071))
svm_lin_df3 <- parSapply(cl, 1:length(cost_vec), FUN = sum_lin)
svm_poly_df3 <- parSapply(cl, 1:length(cost_vec), FUN = sum_poly)
svm_rad_df3 <- parSapply(cl, 1:length(cost_gamma), FUN = sum_rad)
stopCluster(cl)

############################
######## ACCURACY ##########
############################

svm_lin_bestmod <- svm(emails.V1~ ., data = train_df, kernel = "linear", cost = .1,
                       tunecontrol = tune.control(cross = 5))

train_pred <- predict(svm_lin_bestmod, newdata = train_df)
acc_matrix <- table(train_df[,1], train_pred)

total_acc <- sum(diag(acc_matrix)) / nrow(train_df)


############################
######## PREDICTON #########
############################

# Load and modify test data set
test_df <- read.csv("STAT154-Group04/data/processed_test_df_2.csv")
test_df <- test_df[,-1]

col_names_train <- colnames(train_df)
col_names_train <- col_names_train[-1] #remove emails.v1

colnames(test_df)[1:3] <- col_names_train[1:3]
test_df <- test_df[,which(colnames(test_df) %in% col_names_train)]

pred.te <- predict(svm_lin_bestmod, newdata = test_df)
test_pred <- data.frame(pred.te)
write.table(x = pred.te, file = "predict.txt", row.names = FALSE, col.names = FALSE)

############################################################
# Save image in Rdata so it can be loaded in other scripts
save.image(file="STAT154-Group04/data/SVM.Rdata")
load("STAT154-Group04/data/SVM.Rdata")
############################################################
