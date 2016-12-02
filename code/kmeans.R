library("ggplot2")
set.seed(29)

# read in training data and top 100 features and index only those features
trainFull <- read.csv("processed_train_df_2.csv")
top100features <- as.character(read.csv("top100.csv", header = TRUE)$x)
x <- trainFull[top100features]

# run k-means 50 times with randomly initializing assignments
km <- kmeans(x, 5, nstart = 50)
# how many data points assigned to each label in original training set
table(trainFull$emails.V1)
# compare table above to cluster sizes
km
# within-cluster sum of squares measuring cluster compactness
km$tot.withinss

# plot clusters from different two-dimensional angles
km$cluster <- as.factor(km$cluster)
ggplot(x, aes(sid, num_words, color = km$cluster)) + geom_point() + theme(legend.position="none") +
  ggtitle("#1 Best Feature (sid) vs. #3 Best Feature (num_words)")
ggplot(x, aes(fyi, num_words, color = km$cluster)) + geom_point() + theme(legend.position="none") +
  ggtitle("#2 Best Feature (fyi) vs. #3 Best Feature (num_words)")
ggplot(x, aes(tuesday, num_words, color = km$cluster)) + geom_point() + theme(legend.position="none") +
  ggtitle("#25 Best Feature (tuesday) vs. #3 Best Feature (num_words)")
ggplot(x, aes(get, num_words, color = km$cluster)) + geom_point() + theme(legend.position="none") +
  ggtitle("#50 Best Feature (get) vs. #3 Best Feature (num_words)")
ggplot(x, aes(make, num_words, color = km$cluster)) + geom_point() + theme(legend.position="none") +
  ggtitle("#100 Best Feature (make) vs. #3 Best Feature (num_words)")
ggplot(x, aes(sid, make, color = km$cluster)) + geom_point() + theme(legend.position="none") +
  ggtitle("#1 Best Feature (sid) vs. #100 Best Feature (make)")
# top100features[1] = "sid"
# top100features[2] = "fyi"
# top100features[3] = "num_words"
# top100features[25] = "tuesday"
# top100features[50] = "get"
# top100features[100] = "make"