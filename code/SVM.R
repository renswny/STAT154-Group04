############################################################
# SVM
############################################################

# Setwd to root directory of project before reading data
setwd("~/Fall 2016/STAT 154/Final Project/STAT154-Group04")

# Read training data
train_df1 <- read.csv("data/processed_train_df.csv")
train_df2 <- read.csv("data/processed_train_df_2.csv")
train_df3 <- read.csv("data/processed_train_df_3.csv")

train_df1 <- train_df1[,-1]
train_df2 <- train_df2[,-1]
train_df3 <- train_df3[,-1]

train_df1[,1] <- as.factor(train_df1[,1])
train_df2[,1] <- as.factor(train_df2[,1])
train_df3[,1] <- as.factor(train_df3[,1])

# First column is target variable
# Second is number of words 
# Remaining are word frequencies

# Load necessary libraries
library(e1071)
library(parallel)

# Give variables names that are specific to model (SVM_mse instead of mse)

### Functions ###
svm_sum_rad <- function(data_set, cost_vec = 0, gamma_vec = 0, num_folds){
  tune.out <- tune(svm, emails.V1~ ., data = data_set, kernel = "radial",
                   ranges = list(cost = cost_vec, gamma = gamma_vec),
                   tunecontrol = tune.control(cross = num_folds))
  sum_tune.out <- summary(tune.out)
  return(sum_tune.out)
}

svm_sum <- function(kernel_method, data_set, cost_vec = 0, num_folds, num_degree = 0){
  tune.out <- tune(svm, emails.V1~ ., data = data_set, kernel = kernel_method, degree = num_degree,
                   ranges = list(cost = cost_vec),
                   tunecontrol = tune.control(cross = num_folds))
  sum_tune.out <- summary(tune.out)
  return(sum_tune.out)
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

### Global Variables ###
kernels <- c("radial", "linear", "polynomial")
cost_vec <- c(.05, .1, .15, 1, 10)
cost_vec_poly <- c(.05, .1, .15, 1, 10, 100)
gamma_vec <- c(.1, .5, 1)

#### DATA SET 1 ####
sum_svm_rad_1 <- svm_sum_rad(train_df1, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_svm_lin_1 <- svm_sum(kernels[2], train_df1, cost_vec = cost_vec_poly, num_folds = 5)
sum_svm_pol_1 <- svm_sum(kernels[3], train_df1, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)

#### DATA SET 2 ####
sum_svm_rad_2 <- svm_sum_rad(train_df2, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_svm_lin_2 <- svm_sum(kernels[2], train_df2, cost_vec = cost_vec_poly, num_folds = 5)
sum_svm_pol_2 <- svm_sum(kernels[3], train_df2, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)

#### DATA SET 3 ####
sum_svm_rad_3 <- svm_sum_rad(train_df3, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_svm_lin_3 <- svm_sum(kernels[2], train_df3, cost_vec = cost_vec_poly, num_folds = 5)
sum_svm_pol_3 <- svm_sum(kernels[3], train_df3, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)


###### MODIFIED DATA SET 1 ######
# We are only looking at the columns that have more than 5 occurrences
mod5_train_df1 <- rem_columns(train_df1, 3, 5)
mod5_train_df2 <- rem_columns(train_df2, 5, 5)
mod5_train_df3 <- rem_columns(train_df3, 5, 5)

sum_rad1_mod5 <- svm_sum_rad(mod5_train_df1, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_lin1_mod5 <- svm_sum(kernels[2], mod5_train_df1, cost_vec = cost_vec_poly, num_folds = 5)
sum_pol1_mod5 <- svm_sum(kernels[3], mod5_train_df1, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)

sum_rad2_mod5 <- svm_sum_rad(mod5_train_df2, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_lin2_mod5 <- svm_sum(kernels[2], mod5_train_df2, cost_vec = cost_vec_poly, num_folds = 5)
sum_pol2_mod5 <- svm_sum(kernels[3], mod5_train_df2, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)

sum_rad3_mod5 <- svm_sum_rad(mod5_train_df3, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_lin3_mod5 <- svm_sum(kernels[2], mod5_train_df3, cost_vec = cost_vec_poly, num_folds = 5)
sum_pol3_mod5 <- svm_sum(kernels[3], mod5_train_df3, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)


###### MODIFIED DATA SET 2 ######
# We are only looking at the columns that have more than 10 occurrences
mod10_train_df1 <- rem_columns(train_df1, 3, 10)
mod10_train_df2 <- rem_columns(train_df2, 5, 10)
mod10_train_df3 <- rem_columns(train_df3, 5, 10)

sum_rad1_mod10 <- svm_sum_rad(mod10_train_df1, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_lin1_mod10 <- svm_sum(kernels[2], mod10_train_df1, cost_vec = cost_vec_poly, num_folds = 5)
sum_pol1_mod10 <- svm_sum(kernels[3], mod10_train_df1, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)

sum_rad2_mod10 <- svm_sum_rad(mod10_train_df2, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_lin2_mod10 <- svm_sum(kernels[2], mod10_train_df2, cost_vec = cost_vec_poly, num_folds = 5)
sum_pol2_mod10 <- svm_sum(kernels[3], mod10_train_df2, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)

sum_rad3_mod10 <- svm_sum_rad(mod10_train_df3, cost_vec = cost_vec, gamma_vec = gamma_vec, 5)
sum_lin3_mod10 <- svm_sum(kernels[2], mod10_train_df3, cost_vec = cost_vec_poly, num_folds = 5)
sum_pol3_mod10 <- svm_sum(kernels[3], mod10_train_df3, cost_vec = cost_vec_poly, num_folds = 5, num_degree = 2)








############################################################
# Save image in Rdata so it can be loaded in other scripts
save.image(file="data/SVM.Rdata")
############################################################






#Kensen code for Parallel
cl <- makeCluster(4) #use four cores for this job, if you have more cores, use a higher number
clusterExport(cl, "train_df")
clusterExport(cl, "impt")
clusterExport(cl, "ntree")
clusterEvalQ(cl, library(randomForest))
OOBerror <- parSapply(cl, 1:4 , rf) 
stopCluster(cl)
