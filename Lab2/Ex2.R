sample <- readLines("sample2.txt")
sample = as.numeric(sample)
outliers_iqr <- IQR(sample)
samplesummary <- summary(sample)
q1 <- samplesummary[2]
q3 <- samplesummary[4]
lower <- q1 - 1.5 * outliers_iqr
higher <- q3 + 1.5 * outliers_iqr
outliers <- c()
for (value in sample){
  if (value < lower | value > higher){
    outliers <- append(outliers, value)
  }
}
outliers