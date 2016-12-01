library(caret)
library(Boruta)
library(dplyr)

train <- read.csv("data/processed_train_df_2.csv")
set.seed(1234)
idx <- createDataPartition(train$emails.V1,p=0.01,list=FALSE)
sample.df <- train[idx,]
explanatory.attributes <- setdiff(names(sample.df),c("X","emails.V1"))
data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[,x])})

unique.classes <- unique(data.classes)
attr.by.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.by.data.types) <- unique.classes
comment(attr.by.data.types) <- "list that categorize training data types"

pp <- preProcess(sample.df[c(attr.by.data.types$numeric,attr.by.data.types$integer)],
                 method=c("medianImpute"))
pp.sample.df <- predict(pp,sample.df[c(attr.by.data.types$numeric,attr.by.data.types$integer)])

# combine numeric data with character data
df <- cbind(pp.sample.df,sample.df[attr.by.data.types$character])


cn <- colnames(df)
for (i in 1:length(cn)) {
  if (str_detect(cn[i], "shadow")) {
    print(cn[i])
    cn[i] <- paste("X", cn[i])
  }
}
colnames(df) <- cn

bor.results <- Boruta(df,factor(sample.df$emails.V1),
                      maxRuns=18, pValue = 0.4,
                      doTrace=0)
decision <- bor.results$finalDecision
decision <- data.frame(rownames(decision), decision)
tentative <- decision[decision$decision == "Tentative",]

tentativeWords <- tentative$rownames.decision
tentativeWords <- as.vector(tentativeWords)
new_df <- train[, tentativeWords]

powerFeats <- train[ , 1]
for (i in 1:ncol(new_df)) {
  for (j in 1:ncol(new_df)) {
    newvar = paste(colnames(new_df)[i], colnames(new_df)[j], sep = "*")
    x = data.frame(new_df[ , i]*new_df[ , j])
    colnames(x) = newvar
    powerFeats <- data.frame(powerFeats, x)
  }
}

for (i in 1:ncol(new_df)) {
  for (j in 1:ncol(new_df)) {
    for (k in 1:ncol(new_df)) {
      newvar = paste(colnames(new_df)[i], colnames(new_df)[j],colnames(new_df)[k] , sep = "*")
      x = data.frame(new_df[ , i]*new_df[ , j]*new_df[ , k])
      colnames(x) = newvar
      powerFeats <- data.frame(powerFeats, x)
    }
  }
}
 y <- powerFeats[ , -1]
new_df <- data.frame(train[, 1], new_df, powerFeats[ , -1])

write.csv(file="data/processed_train_df_4.csv", new_df)
getwd()
