############################################################
# PROCESSING LDA
############################################################

## Libraries
library(tm)
library(SnowballC)

## Load data
## setwd to root directory


emails <- read.table("data/HRC_train.tsv", header=FALSE, stringsAsFactors = FALSE)
test <- read.table("data/HRC_test.tsv", header=FALSE, stringsAsFactors = FALSE)

# create vectors of words from initial string
for (i in 1:nrow(emails)) {
  emails$wordvec[i] <-  strsplit(emails$V2[i], " ")
}

for (i in 1:nrow(test)) {
  test$wordvec[i] <-  strsplit(test$V1[i], " ")
}

wordvec <- c(emails$wordvec, test$wordvec)


emailsC <- Corpus(VectorSource(wordvec))

# Straightforward processing
processed <- tm_map(emailsC, content_transformer(tolower))
processed <- tm_map(processed, removePunctuation)
processed <- tm_map(processed, removeNumbers)
processed <- tm_map(processed, removeWords, stopwords("english"))

processed <- tm_map(processed, stemDocument, language = "english")

# Most frequent terms "state", "depart", "case", "date", "doc", "subject", "sent", "will", not meaningful.

# Keep only words that are in 3-1305 emails
dtmr <- DocumentTermMatrix(processed, control=list(bounds = list(global=c(3, 1305))))

library(topicmodels)
burnin <- 4000
iter <- 1000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE


#Number of topics
k <- 10

ldaOut <-LDA(dtmr,k, method="Gibbs", 
             control=list(nstart=nstart, 
                          seed = seed, 
                          best=best, burnin = burnin, 
                          iter = iter, thin=thin))


terms(ldaOut)
prob_matrix <- round(head(as.data.frame(ldaOut@gamma)), 2)
colnames(prob_matrix) <- terms(ldaOut)

topic <- topics(ldaOut)




