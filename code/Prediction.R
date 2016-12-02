############################################################
# Model Comparison and Final Prediction
############################################################

# Get predicted MSEs from each model
# Predict with final model



library(randomForest)
library(parallel)
# Read training data
train_df <- read.csv("data/processed_train_df_2.csv")
copy <- train_df
train_df <- train_df[,-1]
train_df$emails.V1 <- factor(train_df$emails.V1)
copy <- train_df
train_df <- train_df[ , -(which(colSums(train_df[, -c(1, 2, 3, 4)]) <=9) + 4)]

test_df <- read.csv("data/processed_test_df_2.csv")
word_matrix <- rbind(train_df[, -c(1, 2, 3, 4)], test_df[, -c(1,2,3)])
word_matrix <- word_matrix[, -(which(colsums(word_matrix) <=9))]
train_df <- cbind(train_df[, c(1:4)], word_matrix[1:3505, ])
test_df <- cbind(test_df[, c(1:3)], word_matrix[3506:3894, ])

email.rf <- randomForest(emails.V1 ~ ., data = train_df,
                         mtry = 99,
                         importance = TRUE, ntree = 200)
pred <- predict(email.rf, test_df, type="class")
write.table(pred, "predict.txt")


write.table(pred, "data/predict.txt")