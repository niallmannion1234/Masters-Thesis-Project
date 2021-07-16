# Create testing and training datasets
sample <- floor(0.80 * nrow(imbalanceddata))
set.seed(567)
train_ind <- sample(seq_len(nrow(imbalanceddata)), size = sample)
training <- imbalanceddata[train_ind, ]
test <- imbalanceddata[-train_ind, ]

# Create training and test dataset for Rmarkdown file
setwd("D:\\AD")
write.csv(test, file= "test_data.csv")
write.csv(trains,"training_data.csv")
