setwd("C:\\Users\\tanke\\Desktop\\STAT154-Group04")

library(randomForest)
library(parallel)
train_df <- read.csv("data/processed_train_df_2.csv") #row.names = TRUE does not work
train_df<- train_df[,-1] #delete the row columns
train_df$emails.V1 <- factor(train_df$emails.V1)  #factorize the sender column so randomforest will do classification

train_df <- train_df[ , -(which(colSums(train_df[,-c(1,2,3,4)]) <= 1) +4)]    #remove words only occured once about 50

train_df_filtered <- train_df[ , -(which(colSums(train_df[,-c(1,2,3,4)]) <= 9) +4)]

test_df <- read.csv("data/processed_test_df_2.csv")
test_df <- test_df[,-1]
colnames(test_df)[1:3] <- colnames(train_df)[2:4]

#------function for CV RF
mod <- function(B, m, i,df){
  
  email.rf <- randomForest(emails.V1~., data = df, subset = unlist(cv_idx[-i]), 
                           mtry = m, 
                           importance = TRUE, ntree = B)
  
  pred <- predict(email.rf, df[cv_idx[[i]],], type = "class")
  conf_tbl <- table(pred, df$emails.V1[cv_idx[[i]]])
  error_rate <- 1-( sum(diag(conf_tbl))/sum(conf_tbl))   #error
  
  return(list(conf_tbl, error_rate))
  
}    

#---------------------------------------
Sys.time() ->x
set.seed(777)
cv_idx <- split(sample(1:nrow(train_df)), 1:5) 
cl <- makeCluster(3) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df_filtered")
clusterEvalQ(cl, library(randomForest))

rf.200.70 <- parLapply(cl, 1:5, mod, df = train_df_filtered, B=200, m=70)

stopCluster(cl)

Sys.time() -x

cv_error.200.70 <- c(rf.200.70[[1]][[2]], rf.200.70[[2]][[2]], rf.200.70[[3]][[2]],
              rf.200.70[[4]][[2]],rf.200.70[[5]][[2]])
#---
Sys.time() ->x
set.seed(777)
cv_idx <- split(sample(1:nrow(train_df)), 1:5) 
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df_filtered")
clusterEvalQ(cl, library(randomForest))

rf.400.70 <- parLapply(cl, 1:5, mod, df = train_df_filtered, B=400, m=70)

stopCluster(cl)

Sys.time() -x

cv_error.400.70 <- c(rf.400.70[[1]][[2]], rf.400.70[[2]][[2]], rf.400.70[[3]][[2]],
                     rf.400.70[[4]][[2]],rf.400.70[[5]][[2]])



#----calculate accuracy by class on the better model
accuracy_by_sender<-list()
for ( i in 1:5){
  
rf.200.70[[i]][[1]] -> mat
rowSums(rf.200.70[[i]][[1]]) -> rowsu
accuracy_by_sender[[i]] <- (diag(mat)/rowsu)
}
rowMeans(as.data.frame(accuracy_by_sender))





#-------------------------------------best model and prediction


#-----------------------top features----------

features <- sort(importance(best_model)[,1], decreasing = TRUE)
top2500features <- names(head(features, 2500))

#-------------------
train_df_selected <- train_df_filtered[, colnames(train_df_filtered)%in% top2500features]

data.frame(emails.V1 = train_df$emails.V1, train_df_selected) -> train_df_selected


Sys.time() ->x
set.seed(777)
cv_idx <- split(sample(1:nrow(train_df)), 1:5) 
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df_selected")
clusterEvalQ(cl, library(randomForest))

rf.200.50.1 <- parLapply(cl, 1:5, mod, df = train_df_selected, B=200, m=50)

stopCluster(cl)

Sys.time() -x



cv_error.200.50.1 <- c(rf.200.50.1[[1]][[2]], rf.200.50.1[[2]][[2]], rf.200.50.1[[3]][[2]],
                     rf.200.50.1[[4]][[2]],rf.200.50.1[[5]][[2]])
cv_error.200.50.1

accuracy_by_sender.1<-list()
for ( i in 1:5){
  
  rf.200.50.1[[i]][[1]] -> mat
  rowSums(rf.200.50.1[[i]][[1]]) -> rowsu
  accuracy_by_sender.1[[i]] <- (diag(mat)/rowsu)
}
rowMeans(as.data.frame(accuracy_by_sender.1))


#------------------------------trying topicmodels

read.csv(file = "10topics.csv") -> ten_topic



topic_fun <- function(i){
  
  df <- data.frame(train_df_selected, (ten_topic[,2])[1:3505])
  
  best.rf_topic <- randomForest(
    emails.V1~., data = df, subset = unlist(cv_idx[-i]),
    mtry = 50, 
    ntree = 200)
  
  pred <- predict(best.rf_topic, df[cv_idx[[i]],], type = "class")
  conf_tbl <- table(pred, df$emails.V1[cv_idx[[i]]])
  error_rate <- 1-( sum(diag(conf_tbl))/sum(conf_tbl))   #error
  
  return(list(conf_tbl, error_rate))
  
}






#---topic 10 Performed not so well
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"ten_topic")
clusterExport(cl, "train_df_selected")
clusterEvalQ(cl, library(randomForest))

best_topic10<- parLapply(cl, 1:5, topic_fun)
stopCluster(cl)
best_topic10


cv_error.best_topic10 <- c(best_topic10[[1]][[2]], best_topic10[[2]][[2]], best_topic10[[3]][[2]],
                          best_topic10[[4]][[2]],best_topic10[[5]][[2]])

mean(cv_error.best_topic10)




accuracy_by_sender.2<-list()
for ( i in 1:5){
  
  best_topic10[[i]][[1]] -> mat
  rowSums(mat) -> rowsu
  accuracy_by_sender.2[[i]] <- (diag(mat)/rowsu)
}
rowMeans(as.data.frame(accuracy_by_sender.2))



#---------------------prediction------------------------


test_df_selected <- test_df[,colnames(test_df) %in% colnames(train_df_selected) ]

ncol(test_df_selected)
best_model <- randomForest(emails.V1~., data = train_df_selected, 
                           mtry = 50, 
                           ntree = 200)

pred <- predict(best_model, test_df_selected, type = "class")
pred <- as.numeric(pred)

write.table(pred, "predict.txt", sep = "\n", row.names = FALSE, col.names = FALSE)