############################################################
# Model Comparison and Final Prediction
############################################################

# Get predicted MSEs from each model
load("data/RF.Rdata")
load("data/SVM.Rdata")
load("data/KM.Rdata")

min(RF_mse, SVM_mse, KM_mse)

# Choose best Model

test_df <- read.csv("/data/processed_test_df")
pred <- predict(best_model, test_df)

write.table(pred, "data/predict.txt")