############################################################
# RF
############################################################

# Setwd to root directory of project before reading data
library(randomForest)
library(parallel)
# Read training data
train_df <- read.csv("data/processed_train_df_2.csv")
copy <- train_df
train_df <- train_df[,-1]
train_df$emails.V1 <- factor(train_df$emails.V1)
copy <- train_df
train_df <- train_df[ , -(which(colSums(train_df[, -c(1, 2, 3, 4)]) <=9) + 4)]

mod <- function(B, m, i, df){
  email.rf <- randomForest(emails.V1 ~ ., data = df, subset = unlist(cv_idx[-i]),
                           mtry = m,
                           importance = TRUE, ntree = B)
  pred <- predict(email.rf, df[cv_idx[[i]], ], type = "class")
  conf_tbl <- table(pred, df$emails.V1[cv_idx[[i]]])
  error_rate <- 1 - (sum(diag(conf_tbl))/sum(conf_tbl))
  return(error_rate)
}

rf <- function(i, df) {
  temp_1 <- c()
  mtry <- c(floor(sqrt(ncol(df))), 2*floor(sqrt(ncol(df))), 4*floor(sqrt(ncol(df))))
  
  for (m in mtry) {
    for (B in c(200, 400, 600)) {
      out <- mod(B=B, m=m, i=i, df=df)
      names(out) <- paste(B, m)
      temp_1 <- c(temp_1, out)
    }
  }
  return(temp_1)
}

Sys.time() -> x
set.seed(777)
cv_idx <- split(sample(1:nrow(train_df)), 1:5)
cl <- makeCluster(4)
clusterExport(cl, "cv_idx")
clusterExport(cl, "train_df")
clusterExport(cl, "mod")
clusterEvalQ(cl, library(randomForest))

full_D_rf.output <- parLapply(cl, 1:5, rf, df = train_df)
stopCluster(cl)

############################################################
# Save image in Rdata so it can be loaded in other scripts
save.image(file="data/RF.Rdata")
############################################################