setwd("C:\\Users\\tanke\\Desktop\\STAT154-Group04")

library(randomForest)
library(parallel)
train_df <- read.csv("data/processed_train_df_2.csv") #row.names = TRUE does not work
train_df<- train_df[,-1] #delete the row columns
train_df$emails.V1 <- factor(train_df$emails.V1)  #factorize the sender column so randomforest will do classification

train_df <- train_df[, -(which(colSums(train_df[,-c(1,2,3,4)]) <= 1) +4)]    #remove words only occured once

#-------------------------------------


  mod <- function(B, m, i,df){
  
     email.rf <- randomForest(emails.V1~., data = df, subset = unlist(cv_idx[-i]), 
                             mtry = m, 
                             importance = TRUE, ntree = B)
    
    pred <- predict(email.rf, df[cv_idx[[i]],], type = "class")
    conf_tbl <- table(pred, df$emails.V1[cv_idx[[i]]])
    error_rate <- 1-( sum(diag(conf_tbl))/sum(conf_tbl))   #error
    
    return(error_rate)
    
  }    

 

#for full D

rf <- function(i, df){
  
  
  temp_l <- c()
  mtry <- c(floor(sqrt(ncol(df))), 2*floor(sqrt(ncol(df))), 4*floor(sqrt(ncol(df))) )
  
  for(m in mtry){
    
    for(B in c(200, 400, 600)){
      
      out <- mod(B=B, m=m, i=i, df = df)
      names(out) <- paste(B, m)
     temp_l <- c(temp_l, out)
      
    }
    
  }
  return(temp_l)
}



#-------------------------------full D
Sys.time() ->x
set.seed(777)
cv_idx <- split(sample(1:nrow(train_df)), 1:5) 
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df")
clusterExport(cl, "mod")
clusterEvalQ(cl, library(randomForest))

full_D_rf.output <- parLapply(cl, 1:5, rf, df = train_df)
stopCluster(cl)



cv_tab <- data.frame(full_D_rf.output)
Sys.time() -x
save.image(file = "RF_final.RData")

which_min <- which.min(rowMeans(cv_tab))


best_B <- strsplit(rownames(cv_tab)[which_min], " ")[[1]][1] 
best_m <- strsplit(row.names(cv_tab)[which_min], " ")[[1]][2]
best_B <- as.numeric(best_B)
best_m <- as.numeric(best_m)

best.rf <- randomForest(
                          emails.V1~., data = train_df, 
                           mtry = best_m, 
                           importance = TRUE, ntree = best_B)  #run the model with best B and m
  
AllRank <- importance(best.rf)

rank_impt <- sort((AllRank[,7]), decreasing = FALSE)

unwanted_impt <- rank_impt[1:2285]


train_df.1 <- train_df[ ,-(which(colnames(train_df) %in% names(unwanted_impt)))]

#-------75%---------------------------------------------------------------------------
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df.1")
clusterExport(cl, "mod")
clusterEvalQ(cl, library(randomForest))

full_D_rf.output.1 <- parLapply(cl, 1:5, rf, df = train_df.1)
stopCluster(cl)


cv_tab.1 <- data.frame(full_D_rf.output.1)
save.image(file = "RF_final.RData")
which_min.1 <- which.min(rowMeans(cv_tab.1))

best_B.1 <- strsplit(row.names(cv_tab.1)[which_min.1], " ")[[1]][1] 
best_m.1 <- strsplit(row.names(cv_tab.1)[which_min.1], " ")[[1]][2]
best_B.1 <- as.numeric(best_B.1)
best_m.1 <- as.numeric(best_m.1)

best.rf.1 <- randomForest(
  emails.V1~., data = train_df.1, 
  mtry = best_m.1, 
  importance = TRUE, ntree = best_B.1)

AllRank.1 <- importance(best.rf.1)


rank_impt.1 <- sort((AllRank.1[,7]), decreasing = FALSE)

unwanted_impt.1 <- rank_impt.1[1:2285]


train_df.2 <- train_df.1[ ,-(which(colnames(train_df.1) %in% names(unwanted_impt.1)))]


#----------50%-----------------------------------------------------------------------------
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df.2")
clusterExport(cl, "mod")
clusterEvalQ(cl, library(randomForest))

full_D_rf.output.2 <- parLapply(cl, 1:5, rf, df = train_df.2)
stopCluster(cl)


cv_tab.2 <- data.frame(full_D_rf.output.2)
which_min.2 <- which.min(rowMeans(cv_tab.2))


best_B.2 <- strsplit(rownames(cv_tab.2)[which_min.1], " ")[[1]][1] 
best_m.2 <- strsplit(rownames(cv_tab.2)[which_min.1], " ")[[1]][2] 

best_B.2 <- as.numeric(best_B.2)
best_m.2 <- as.numeric(best_m.2)


best.rf.2 <- randomForest(
  emails.V1~., data = train_df.2, 
  mtry = best_m.2, 
  importance = TRUE, ntree = best_B.2)

AllRank.2 <- importance(best.rf.2)


rank_impt.2 <- sort((AllRank.2[,7]), decreasing = FALSE)

unwanted_impt.2 <- rank_impt.2[1:2285]


train_df.3 <- train_df.2[ ,-(which(colnames(train_df.2) %in% names(unwanted_impt.2)))]

save.image(file = "RF_final.RData")


#------25%
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df.3")
clusterExport(cl, "mod")
clusterEvalQ(cl, library(randomForest))

full_D_rf.output.3 <- parLapply(cl, 1:5, rf, df = train_df.3)
stopCluster(cl)


cv_tab.3 <- data.frame(full_D_rf.output.3)
which_min.3 <- which.min(rowMeans(cv_tab.3))

best_B.3 <- strsplit(rownames(cv_tab.3)[which_min.3], " ")[[1]][1] 
best_m.3 <- strsplit(rownames(cv_tab.3)[which_min.3], " ")[[1]][2] 

best_B.3 <- as.numeric(best_B.3)
best_m.3 <- as.numeric(best_m.3)

best.rf.3 <- randomForest(
  emails.V1~., data = train_df.3, 
  mtry = best_m.3, 
  importance = TRUE, ntree = best_B.3)

AllRank.3 <- importance(best.rf.3)


rank_impt.3 <- sort((AllRank.3[,7]), decreasing = FALSE)

unwanted_impt.3 <- rank_impt.3[1:914]

train_df.4 <- train_df.3[ ,-(which(colnames(train_df.3) %in% names(unwanted_impt.3)))]

#-----15%
Sys.time() -> x
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"train_df.4")
clusterExport(cl, "mod")
clusterEvalQ(cl, library(randomForest))

full_D_rf.output.4<- parLapply(cl, 1:5, rf, df = train_df.4)
stopCluster(cl)


cv_tab.4 <- data.frame(full_D_rf.output.4)

which_min.4 <- which.min(rowMeans(cv_tab.4))

best_B.4 <- strsplit(rownames(cv_tab.4)[which_min.4], " ")[[1]][1] 
best_m.4 <- strsplit(rownames(cv_tab.4)[which_min.4], " ")[[1]][2] 

best_B.4 <- as.numeric(best_B.4)
best_m.4 <- as.numeric(best_m.4)


best.rf.4 <- randomForest(
  emails.V1~., data = train_df.4, 
  mtry = best_m.4, 
 ntree = best_B.4)

save.image(file = "RF_final.RData")

#------------------------------trying topicmodels

read.csv(file = "7topics.csv") -> seven_topic
read.csv(file = "15topics.csv") -> fifthteen_topic
read.csv(file = "20topics.csv") -> twenty3_topic
#combine the topic 


topic_fun <- function(i, topic){
  
  
  if(topic == 7){
    train_df.3_copy <- train_df.4
    train_df.3_copy$topic_7 <- seven_topic[,3]
  }
  
  if(topic == 15){
    train_df.3_copy <- train_df.4
    train_df.3_copy$topic_15 <- fifthteen_topic[,3]
  }
  
  if(topic == 23){
    train_df.3_copy <- train_df.4
    train_df.3_copy$topic_23 <- twenty3_topic[1:1200,]
  }
  if(topic == 0){
    train_df.3_copy <- train_df.4
    train_df.3_copy$topic_23 <- twenty3_topic[1:1200,]
    train_df.3_copy$topic_15 <- fifthteen_topic[,3]
    train_df.3_copy$topic_7 <- seven_topic[,3]
    
  }
  
  best.rf_topic <- randomForest(
    emails.V1~., data = train_df.3_copy, subset = unlist(cv_idx[-i]),
    mtry = best_m.4, 
    importance = TRUE, ntree = best_B.4)
  
  pred <- predict(best.rf_topic, train_df.3_copy[cv_idx[[i]],], type = "class")
  conf_tbl <- table(pred, train_df.3_copy$emails.V1[cv_idx[[i]]])
  error_rate <- 1-( sum(diag(conf_tbl))/sum(conf_tbl))   #error
  
  return(error_rate)
  
}

#---topic 15
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"seven_topic")
clusterExport(cl, "fifthteen_topic")
clusterExport(cl, "train_df.4")
clusterExport(cl, "best_m.4")
clusterExport(cl,"best_B.4")
clusterEvalQ(cl, library(randomForest))

best_topic_15<- parLapply(cl, 1:5, topic_fun, topic = 15)
stopCluster(cl)
mean(unlist(best_topic_15))

train_topic15 <- train_df.4
train_topic15$topic15 <- fifthteen_topic[,3]

best_topic15.rf  <- randomForest(
  emails.V1~., data = train_topic15,
  mtry = best_m.4, 
  ntree = best_B.4)

best_topic15.rf #32.75
tail(importance(best_topic15.rf)) #MDGini 13
#--------------

cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"seven_topic")
clusterExport(cl, "fifthteen_topic")
clusterExport(cl, "train_df.4")
clusterExport(cl, "best_m.4")
clusterExport(cl,"best_B.4")
clusterEvalQ(cl, library(randomForest))

best_topic_7<- parLapply(cl, 1:5, topic_fun, topic = 7)
stopCluster(cl)

mean(unlist(best_topic_7))


train_topic7 <- train_df.4
train_topic7$topic7 <- seven_topic[,3]

best_topic7.rf  <- randomForest(
  emails.V1~., data = train_topic7,
  mtry = best_m.4, 
  ntree = best_B.4) #OOB 32.33%

tail(importance(best_topic7.rf)) #9.535 MDGini

#------
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"seven_topic")
clusterExport(cl, c("fifthteen_topic","twenty3_topic"))
clusterExport(cl, "train_df.4")
clusterExport(cl, "best_m.4")
clusterExport(cl,"best_B.4")
clusterEvalQ(cl, library(randomForest))

best_topic_23<- parLapply(cl, 1:5, topic_fun, topic = 23)
stopCluster(cl)

mean(unlist(best_topic_23))

train_topic23 <- train_df.4
train_topic23$topic23 <- twenty3_topic[1:1200,]

best_topic23.rf  <- randomForest(
  emails.V1~., data = train_topic23,
  mtry = best_m.4, 
  ntree = best_B.4) #OOB 32.83%

tail(importance(best_topic23.rf)) #11 MDGini

#--------------------------------------------------------------------------------







#-----
cl <- makeCluster(4) 
clusterExport(cl,"cv_idx")
clusterExport(cl,"seven_topic")
clusterExport(cl, c("fifthteen_topic", "twenty3_topic"))
clusterExport(cl, "train_df.4")
clusterExport(cl, "best_m.4")
clusterExport(cl,"best_B.4")
clusterEvalQ(cl, library(randomForest))

best_topic_all<- parLapply(cl, 1:5, topic_fun, topic = 0) #all topic columns
stopCluster(cl)

mean(unlist(best_topic_all))

