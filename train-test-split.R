#make sure you've ran data-preparatinon and data-exploration prior to running this

train_validation_index <- sample(1:nrow(PCOS_data), size = as.integer(0.80 * nrow(PCOS_data)), replace = FALSE)

train_index <- sample(train_validation_index, size = as.integer(0.80 * length(train_validation_index)), replace = FALSE)
validation_index <- train_validation_index[!train_validation_index %in% train_index]
test_index <- c(1:nrow(PCOS_data))[!c(1:nrow(PCOS_data)) %in% train_validation_index]

train_data <- PCOS_data[train_index,]
validation_data <- PCOS_data[validation_index,]
test_data <- PCOS_data[test_index,]

#observe PCOS variable distribution between datasets
par(mfrow= c(1,3))
for(feature in colnames(train_data))
{
  hist(train_data[, feature],
       xlab = feature,
       ylab = "Frequency",
       main = sprintf("Distribution of %s on train data", feature))
  
  hist(validation_data[, feature],
       xlab = feature,
       ylab = "Frequency",
       main = sprintf("Distribution of %s on validation data", feature))
  
  hist(test_data[, feature],
       xlab = feature,
       ylab = "Frequency",
       main = sprintf("Distribution of %s on test data", feature))
}