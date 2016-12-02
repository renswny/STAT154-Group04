

library(ggplot2)
load("data/RF_Final.Rdata")

# 0
rf_cv <- data.frame(cv_tab)
params <- rownames(rf_cv)
tr <- c()
m <- c()
for (i in 1:length(params)) {
  p <- strsplit(params[i], " ")[[1]]
  tr <- c(tr, as.numeric(p[1]))
  m <- c(m, as.numeric(p[2]))
}

rf_cv$tr <- tr
rf_cv$m <- m
df <- data.frame()

for (i in 1:nrow(rf_cv)) {
  tdf <- data.frame()
  CV_Error <- t((rf_cv[i, c(1:5)]))
  CV_Error <- unname(CV_Error)
  CV_Error <- c(CV_Error, mean(CV_Error))
  Trees <- rep(rf_cv[i, "tr"], 6)
  M <- rep(rf_cv[i, "m"], 6)
  Mean <- c(rep(0, 5), 1)
  tdf <- data.frame(Trees, M, CV_Error, Mean)
  df <- rbind(df, tdf)
}

mean_df <- df[df$Mean == 1, ]
png("images/rf_cv.png")

ggplot(df, aes(x = Trees, y = CV_Error, col = as.factor(M))) +
  geom_point(aes(size = as.factor(Mean), shape = as.factor(Mean))) + 
  geom_line(aes(x = Trees, y = CV_Error, col = as.factor((M))), data = mean_df) + 
  scale_x_continuous(breaks = c(200, 400, 600)) +
  guides(col=guide_legend(title="M")) + guides(size=FALSE) +
  guides(shape=guide_legend(title="Mean"))

dev.off()

# 1
rf_cv <- data.frame(cv_tab.1)
params <- rownames(rf_cv)
tr <- c()
m <- c()
for (i in 1:length(params)) {
  p <- strsplit(params[i], " ")[[1]]
  tr <- c(tr, as.numeric(p[1]))
  m <- c(m, as.numeric(p[2]))
}

rf_cv$tr <- tr
rf_cv$m <- m
df <- data.frame()

for (i in 1:nrow(rf_cv)) {
  tdf <- data.frame()
  CV_Error <- t((rf_cv[i, c(1:5)]))
  CV_Error <- unname(CV_Error)
  CV_Error <- c(CV_Error, mean(CV_Error))
  Trees <- rep(rf_cv[i, "tr"], 6)
  M <- rep(rf_cv[i, "m"], 6)
  Mean <- c(rep(0, 5), 1)
  tdf <- data.frame(Trees, M, CV_Error, Mean)
  df <- rbind(df, tdf)
}

mean_df <- df[df$Mean == 1, ]
png("images/rf_cv_1.png")

ggplot(df, aes(x = Trees, y = CV_Error, col = as.factor(M))) +
  geom_point(aes(size = as.factor(Mean), shape = as.factor(Mean))) + 
  geom_line(aes(x = Trees, y = CV_Error, col = as.factor((M))), data = mean_df) + 
  scale_x_continuous(breaks = c(200, 400, 600)) +
  guides(col=guide_legend(title="M")) + guides(size=FALSE) +
  guides(shape=guide_legend(title="Mean"))

dev.off()


# 2
rf_cv <- data.frame(cv_tab.2)
params <- rownames(rf_cv)
tr <- c()
m <- c()
for (i in 1:length(params)) {
  p <- strsplit(params[i], " ")[[1]]
  tr <- c(tr, as.numeric(p[1]))
  m <- c(m, as.numeric(p[2]))
}

rf_cv$tr <- tr
rf_cv$m <- m
df <- data.frame()

for (i in 1:nrow(rf_cv)) {
  tdf <- data.frame()
  CV_Error <- t((rf_cv[i, c(1:5)]))
  CV_Error <- unname(CV_Error)
  CV_Error <- c(CV_Error, mean(CV_Error))
  Trees <- rep(rf_cv[i, "tr"], 6)
  M <- rep(rf_cv[i, "m"], 6)
  Mean <- c(rep(0, 5), 1)
  tdf <- data.frame(Trees, M, CV_Error, Mean)
  df <- rbind(df, tdf)
}

mean_df <- df[df$Mean == 1, ]
png("images/rf_cv_2.png")

ggplot(df, aes(x = Trees, y = CV_Error, col = as.factor(M))) +
  geom_point(aes(size = as.factor(Mean), shape = as.factor(Mean))) + 
  geom_line(aes(x = Trees, y = CV_Error, col = as.factor((M))), data = mean_df) + 
  scale_x_continuous(breaks = c(200, 400, 600)) +
  guides(col=guide_legend(title="M")) + guides(size=FALSE) +
  guides(shape=guide_legend(title="Mean"))
dev.off()


# 3
rf_cv <- data.frame(cv_tab.3)
params <- rownames(rf_cv)
tr <- c()
m <- c()
for (i in 1:length(params)) {
  p <- strsplit(params[i], " ")[[1]]
  tr <- c(tr, as.numeric(p[1]))
  m <- c(m, as.numeric(p[2]))
}

rf_cv$tr <- tr
rf_cv$m <- m
df <- data.frame()

for (i in 1:nrow(rf_cv)) {
  tdf <- data.frame()
  CV_Error <- t((rf_cv[i, c(1:5)]))
  CV_Error <- unname(CV_Error)
  CV_Error <- c(CV_Error, mean(CV_Error))
  Trees <- rep(rf_cv[i, "tr"], 6)
  M <- rep(rf_cv[i, "m"], 6)
  Mean <- c(rep(0, 5), 1)
  tdf <- data.frame(Trees, M, CV_Error, Mean)
  df <- rbind(df, tdf)
}

mean_df <- df[df$Mean == 1, ]
png("images/rf_cv_3.png")

ggplot(df, aes(x = Trees, y = CV_Error, col = as.factor(M))) +
  geom_point(aes(size = as.factor(Mean), shape = as.factor(Mean))) + 
  geom_line(aes(x = Trees, y = CV_Error, col = as.factor((M))), data = mean_df) + 
  scale_x_continuous(breaks = c(200, 400, 600)) +
  guides(col=guide_legend(title="M")) + guides(size=FALSE) +
  guides(shape=guide_legend(title="Mean"))
dev.off()


# 4
rf_cv <- data.frame(cv_tab.4)
params <- rownames(rf_cv)
tr <- c()
m <- c()
for (i in 1:length(params)) {
  p <- strsplit(params[i], " ")[[1]]
  tr <- c(tr, as.numeric(p[1]))
  m <- c(m, as.numeric(p[2]))
}

rf_cv$tr <- tr
rf_cv$m <- m
df <- data.frame()

for (i in 1:nrow(rf_cv)) {
  tdf <- data.frame()
  CV_Error <- t((rf_cv[i, c(1:5)]))
  CV_Error <- unname(CV_Error)
  CV_Error <- c(CV_Error, mean(CV_Error))
  Trees <- rep(rf_cv[i, "tr"], 6)
  M <- rep(rf_cv[i, "m"], 6)
  Mean <- c(rep(0, 5), 1)
  tdf <- data.frame(Trees, M, CV_Error, Mean)
  df <- rbind(df, tdf)
}

mean_df <- df[df$Mean == 1, ]
png("images/rf_cv_4.png")

ggplot(df, aes(x = Trees, y = CV_Error, col = as.factor(M))) +
  geom_point(aes(size = as.factor(Mean), shape = as.factor(Mean))) + 
  geom_line(aes(x = Trees, y = CV_Error, col = as.factor((M))), data = mean_df) + 
  scale_x_continuous(breaks = c(200, 400, 600)) +
  guides(col=guide_legend(title="M")) + guides(size=FALSE) +
  guides(shape=guide_legend(title="Mean"))
dev.off()

load("data/output.Rdata")
View(vec1)
v <- as.matrix(vec1)
rownames(vec1)

write.csv(file = "data/importance.csv", vec1)
vec1 <- read.csv("data/importance.csv")


load("data/SVM.Rdata")


