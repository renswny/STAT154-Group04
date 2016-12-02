library(ggplot2)
rf_cv <- read.csv("data/cv_tab.csv")

df <- data.frame()
for (i in 1:nrow(rf_cv)) {
  tdf <- data.frame()
  CV_Error <- t((rf_cv[i, c(3:7)]))
  CV_Error <- unname(CV_Error)
  CV_Error <- c(CV_Error, mean(CV_Error))
  means <- rep(mean(CV_Error), 6)
  Trees <- rep(rf_cv[i, 1], 6)
  M <- rep(rf_cv[i, 2], 6)
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
  guides(col=guide_legend(title="M")) + guides(size=FALSE) + guides(shape=FALSE)

dev.off()

