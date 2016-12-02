############################################################
# DATA PROCESSING
############################################################

############################################################
# 1. Turning email strings into a word frequency matrix
############################################################

# Used a tutorial for tm package
# Can be found at https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

## Libraries
library(tm)
library(stringr)
library(SnowballC)
library(wordcloud)

## Load data
## setwd to root directory
emails <- read.table("data/HRC_train.tsv", sep="\t", header=FALSE, stringsAsFactors = FALSE)
test <- read.table("data/HRC_test.tsv", sep = "\t", header=FALSE, stringsAsFactors = FALSE)

# create vectors of words from initial string
for (i in 1:nrow(emails)) {
  emails$wordvec[i] <-  strsplit(emails$V2[i], " ")
}

for (i in 1:nrow(test)) {
  test$wordvec[i] <-  strsplit(test$V1[i], " ")
}

wordvec <- c(emails$wordvec, test$wordvec)

# Create "corpus" that tm_map takes as input
# Need to create one feature matrix for both train and test set
emailsC <- Corpus(VectorSource(wordvec))

# Straightforward processing
processed <- tm_map(emailsC, content_transformer(tolower))
processed <- tm_map(processed, removePunctuation)
processed <- tm_map(processed, removeNumbers)
dtm_raw <- DocumentTermMatrix(processed)
processed <- tm_map(processed, removeWords, stopwords("english"))
dtm_stop <- DocumentTermMatrix(processed)
processed <- tm_map(processed, stemDocument, language = "english")
dtm_stem <- DocumentTermMatrix(processed)
freq <- colSums(as.matrix(dtm_stem))
ord <- order(freq,decreasing=TRUE)
freq[head(ord, n = 20)]

# Most frequent terms "state", "depart", "case", "date", "doc", "subject", "sent", "will", not meaningful.

# Keep only words that are in within a frequency bound
dtmr <- DocumentTermMatrix(processed, control=list(bounds = list(global=c(3, 1200))))
freqr <- colSums(as.matrix(dtmr))
ordr <- order(freqr,decreasing=TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]




############################################################
# 2. Exploring other possible features from training set
############################################################

# Exploration
library(ggplot2)
png("images/SenderHistogram.png")
qplot(V1, data=emails, geom="histogram", col=I("white"), binwidth=1,
              main = "Histogram for Sender",
              xlab = "Sender")
dev.off()
png("images/SenderNchar.png")
plot(nchar(emails$V2), emails$V1, ylab="Sender", xlab ="Characters per Email",
     main="Characters per Email by Sender")
dev.off()

# Compute some other statistics about each email, see if they might be useful
total_chars <- c()
mean_chars <- c()
num_words <- c()
ampersands <- c()
qmarks <- c()
semicolons <- c()
qmarks_per_word <- c()
semicolons_per_word <- c()
uppercase_per_word <- c()
periods <- c()
commas <- c()
hyphens <- c()
parentheses <- c()
numerals <- c()
for (i in 1:nrow(emails)) {
  vec <- as.vector((emails$wordvec[i])[[1]])
  total_chars[i] <- sum(nchar(vec))
  mean_chars[i] <- mean(nchar(vec))
  num_words[i] <- length(vec)
  ampersands[i] <-str_count(emails$V2[i], pattern = "&" )
  qmarks[i] <-str_count(emails$V2[i], pattern = "/?" )
  qmarks_per_word[i] <- qmarks[i]/num_words[i]
  semicolons[i] <-str_count(emails$V2[i], pattern = ";" )
  semicolons_per_word[i] <- semicolons[i]/num_words[i]
  periods[i] <-str_count(emails$V2[i], pattern = ".")
  commas[i] <- str_count(emails$V2[i], pattern = ",")
  hyphens[i] <- str_count(emails$V2[i], pattern = "-")
  parentheses[i] <- str_count(emails$V2[i], pattern = "\\(") + str_count(emails$V2[i], pattern = "\\)")
  numerals[i] <- str_count(emails$V2[i], pattern = "[0-9]")
}

# Significant
summary(aov(total_chars ~ emails$V1))

# Significant
summary(aov(mean_chars ~ emails$V1))

# Significant - related to total_chars
summary(aov(num_words ~ emails$V1))

# Not significant
summary(aov(ampersands ~ emails$V1))

# Significant
summary(aov(qmarks ~ emails$V1))
# Significant
summary(aov(qmarks_per_word ~ emails$V1))

# Not significant
summary(aov(semicolons_per_word ~ emails$V1))

# Not significant
summary(aov(uppercase_per_word ~ emails$V1))

# Not significant
words_per_sentence <- num_words/periods
summary(aov(words_per_sentence ~ emails$V1))

# Not significant
words_per_comma <- commas/num_words
summary(aov(words_per_comma ~ emails$V1))


# Not Significant
parentheses_per_word <- parentheses/num_words
summary(aov(parentheses_per_word ~ emails$V1))

# Not Significant
numerals_per_word <- numerals/num_words
summary(aov(numerals_per_word ~ emails$V1))

# Wasn't able to find anything too distinct in writing styles
# But will add number of words in email as a feature

############################################################
# 3. Creating full dataframe
############################################################

# Create df for analysis
m <- as.matrix(dtmr)
m_train <- m[1:1200,]
df <- data.frame(emails$V1, num_words, m_train)

# Write base set to csv
write.csv(df, file="wrongdata/data/processed_train_df.csv")

# After adding new features
df2 <- data.frame(emails$V1, num_words, qmarks_per_word,
                  hyphens_per_word, m_train)
write.csv(df2, file="wrongdata/data/processed_train_df_2.csv")

# Process Test Data for use in final submission
m_test <- m[1201:1305,]
test_num_words <- c()
test_qmarks_per_word <- c()
test_hyphens_per_word <- c()
for (i in 1:nrow(test)) {
  vec <- as.vector((test$wordvec[i])[[1]])
  test_num_words[i] <- length(vec)
  test_hyphens_per_word[i] <- str_count(test$wordvec[i], pattern = "-")/test_num_words[i]
  test_qmarks_per_word[i] <- str_count(test$wordvec[i], pattern = "/?")/test_num_words[i]
}
df_test <- data.frame(test_num_words, m_test)
write.csv(df_test, file="wrongdata/data/processed_test_df.csv")

# After adding some additional features: our best model
df_test_2 <- data.frame(test_num_words, test_qmarks_per_word, 
                      test_hyphens_per_word, m_test)
write.csv(df_test_2, file="wrongdata/data/processed_test_df_2.csv")

############################################################
# 4. FEATURE REDUCTION: UNIVARIATE ANOVA
############################################################


# Try computing one-way significance to produce reduced feature set
word_aov <- c()
words <- as.vector(colnames(m))
for (i in 1:ncol(m)) {
  col <- m_train[,i]
  summ <- summary(aov(col ~ emails$V1))
  p <- summ[[1]][["Pr(>F)"]][1]
  word_aov <- c(word_aov, p)
}

aov_df <- data.frame(words, word_aov)

# Take only words with p-values less than 5%
sig_df <- aov_df[aov_df$word_aov < .05, ]
sig_cols <- sig_df$words
sig_dtmr <- m[, sig_cols]

df3 <- data.frame(emails$V1, num_words, numerals_per_word,
                  hyphens_per_word, sig_dtmr[1:3505,])
write.csv(df3, file="wrongdata/data/processed_train_df_3.csv")

df_test_3 <- data.frame(test_num_words, test_qmarks_per_word, 
                        test_hyphens_per_word, sig_dtmr[3506:3894,])
write.csv(df_test_3, file="wrongdata/data/processed_test_df_3.csv")


###########################################################
# 5. BORUTA FEATURE SELECTION
# With ~9000 features, we try to run a feature selection method
############################################################

library(caret)
library(Boruta)
library(dplyr)

train <- read.csv("data/processed_train_df_2.csv")
set.seed(1234)
idx <- createDataPartition(train$emails.V1,p=0.01,list=FALSE)

# Take small sample of the data
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
df <- cbind(pp.sample.df,sample.df[attr.by.data.types$character])

# Change the colnames that begin with "shadow" as it throws an error
cn <- colnames(df)
for (i in 1:length(cn)) {
  if (str_detect(cn[i], "shadow")) {
    print(cn[i])
    cn[i] <- paste("X", cn[i])
  }
}
colnames(df) <- cn

# Run Boruta 
bor.results <- Boruta(df,factor(sample.df$emails.V1),
                      maxRuns=18, pValue = 0.4,
                      doTrace=0)

# Unfortunately, no features are confirmed as important.
# Only about 10 are considered possibly important, the rest are confirmed unimportant
# We raised the p-value in the hopes of getting more features, but got similar results

decision <- bor.results$finalDecision
decision <- as.matrix(decision)
decision <- data.frame(rownames(decision), decision)
tentative <- decision[decision$decision == "Tentative",]
tentativeWords <- tentative$rownames.decision
tentativeWords <- as.vector(tentativeWords)
new_df <- train[, tentativeWords]

# From these 10, we compute quadratic and cubic powers
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

# Contains about 1100 features
write.csv(file="wrongdata/data/processed_train_df_4.csv", new_df)




