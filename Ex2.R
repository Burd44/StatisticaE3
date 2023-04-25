s = sd(sample)
outliers_mean <- mean(sample)
lower <- outliers_mean - 2 * s
higher <- outliers_mean + 2 * s
outliers <- c()
for (value in sample){
  if (value < lower | value > higher){
    outliers <- append(outliers, value)
  }
}
outliers
