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
test <- read.table("data/HRC_test.tsv", sep="\t", header=FALSE, stringsAsFactors = FALSE)

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
# 38066 terms
processed <- tm_map(processed, removeWords, stopwords("english"))
dtm_stop <- DocumentTermMatrix(processed)
# 37966 terms
processed <- tm_map(processed, stemDocument, lazy=TRUE)
dtm_stem <- DocumentTermMatrix(processed)
# 26177 terms
freq <- colSums(as.matrix(dtm_stem))
ord <- order(freq,decreasing=TRUE)
freq[head(ord, n = 20)]

# Most frequent terms "state", "depart", "case", "date", "doc", "subject", "sent", "will", not meaningful.

# Keep only words that are in 3-1200 emails (least common word eliminated by this is "subject"):
dtmr <- DocumentTermMatrix(processed, control=list(bounds = list(global=c(3, 1200))))
# 8686 terms
freqr <- colSums(as.matrix(dtmr))
ordr <- order(freqr,decreasing=TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]

set.seed(1234)
# Break up by sender
for (i in 1:5) {
  dtrmi <-  dtmr[emails$V1 == as.character(i), ]
  assign(paste("dtmr",i, sep=""), dtrmi)
  freqi <- colSums(as.matrix(dtrmi))
  assign(paste("freq",i, sep=""), freqi)
}

# Wordclouds
set.seed(1234)
wordcloud(names(freq1), freq1, min.freq=200)
wordcloud(names(freq2), freq2, min.freq=200)
wordcloud(names(freq3), freq3, min.freq=200)
wordcloud(names(freq4), freq4, min.freq=200)
wordcloud(names(freq5), freq5, min.freq=200)

############################################################
# 2. Exploring other possible features from training set
############################################################

# Exploration
hist(emails$V1)
plot(nchar(emails$V2), emails$V1)

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
  uppercase_per_word[i] <- str_count(emails$V2[i], pattern = "[A-Z]" )/num_words[i]
}

# Significant
summary(aov(total_chars ~ emails$V1))

# Not significant
summary(aov(mean_chars ~ emails$V1))

# Significant - related to total_chars
summary(aov(num_words ~ emails$V1))

# Not significant
summary(aov(ampersands ~ emails$V1))

# Significant
summary(aov(qmarks ~ emails$V1))
# Not significant - was related to length
summary(aov(qmarks_per_word ~ emails$V1))

# Not significant
summary(aov(semicolons_per_word ~ emails$V1))

# Not significant
summary(aov(uppercase_per_word ~ emails$V1))

# Wasn't able to find anything too distinct in writing styles
# But will add number of words in email as a feature

############################################################
# 3. Creating final dataframe
############################################################

# Create df for analysis
m <- as.matrix(dtmr)
m_train <- m[1:1200,]
df <- data.frame(emails$V1, num_words, m_train)

# Write base set to csv
write.csv(df, file="data/processed_train_df.csv")


# Process Test Data for use in final submission
m_test <- m[1201:1305,]
for (i in 1:nrow(test)) {
  vec <- as.vector((test$wordvec[i])[[1]])
  test$num_words[i] <- length(vec)
}
df_test <- data.frame(test$num_words, m_test)
write.csv(df_test, file="data/processed_test_df.csv")
