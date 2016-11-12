############################################################
# RF
############################################################

# Setwd to root directory of project before reading data

# Read training data
train_df <- read.csv("data/processed_train_df.csv")
# First column is target variable
# Second is number of words 
# Remaining are word frequencies

# Give variables names that are specific to model (RF_mse instead of mse)





############################################################
# Save image in Rdata so it can be loaded in other scripts
save.image(file="data/RF.Rdata")
############################################################