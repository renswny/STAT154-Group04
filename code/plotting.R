

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
  guides(shape=guide_legend(title="Mean")) +
  ggtitle("Cross-Validation at 9792 Features")

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
  guides(shape=guide_legend(title="Mean")) +
  ggtitle("Cross-Validation at 7344 Features")

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
  guides(shape=guide_legend(title="Mean")) +
  ggtitle("Cross Validation at 4896 Features")
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
  guides(shape=guide_legend(title="Mean")) +
  ggtitle("Cross Validation at 2448 Features")
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
  guides(shape=guide_legend(title="Mean"))  +
  ggtitle("Cross Validation at 1469 Features")
dev.off()



load("data/SVM.Rdata")
View(svm_poly_df3)
View(svm_rad_df3)
rownames(as.matrix(svm_rad_df3))
unname(as.matrix(svm_rad_df3))
svm_rad <- data.frame("params" = rownames(as.matrix(svm_rad_df3)), 
                      "Mean CV Error" = unname(as.matrix(svm_rad_df3)))
rownames(svm_rad)
rad_Cost = rep(c(0.1, 0.5, 1, 10, 100), each=4)
rad_Gamma = rep(c(0.1, 0.5, 1, 5), 5)

df_rad <- data.frame("Cost" = rad_Cost,
                     "Method" = rep("Radial", 20),
                     "Mean CV Error" = svm_rad$Mean.CV.Error, 
                     "Gamma" = rad_Gamma
                     )

df_poly <- data.frame("Cost" = c(0.1, 0.5, 1, 10, 100), 
                      "Method" = rep("Poly", 5),
                      "Mean CV Error" = unname(svm_poly_df3),
                      "Gamma" = rep(NA, 5 ))

df_lin <- data.frame("Cost" = c(0.1, 0.5, 1, 10, 100), 
                      "Method" = rep("Linear", 5),
                      "Mean CV Error" = unname(svm_lin_df3), 
                     "Gamma" = rep(NA, 5 ))

svm_err <- rbind(df_lin, df_poly, df_rad)


png("images/SVM_CV.png")
ggplot(svm_err, aes(x = Cost, y = Mean.CV.Error, col = as.factor(Method))) +
  geom_point() + 
  geom_line() + 
  guides(col=guide_legend(title="Method"))

dev.off()



png("images/Radial_CV.png")
ggplot(df_rad, aes(x = Cost, y = Mean.CV.Error, col = as.factor(Gamma))) +
  geom_point() + 
  geom_line() + 
  guides(col=guide_legend(title="Gamma"))

dev.off()
