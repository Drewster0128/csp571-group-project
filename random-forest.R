library(tree)
library(randomForest)
library(pROC)

#rename features
names(PCOS_data) <- sapply(names(PCOS_data), function(column_name) {
  column_name <- gsub(" ", "", column_name)
  column_name <- gsub("\\(", "", column_name)
  column_name <- gsub("\\)", "", column_name)
  column_name <- gsub("\\/", "", column_name)
  column_name <- gsub("\\-", "", column_name)
  column_name <- gsub("\\:", "", column_name)
  column_name <- gsub("\\`", "", column_name)
  return(column_name)
})

#convert categorical variables to factors
#perform forward selection
available_predictors <- colnames(PCOS_data)[!colnames(PCOS_data) %in% c("PCOSYN")]
chosen_predictors <- c()
chosen_models <- c()


for(i in 1:(ncol(PCOS_data) -1))
{
  max_precision <- NULL
  max_predictors <- NULL
  max_forest <- NULL
  for(x in available_predictors)
  {
    rf <- randomForest(as.factor(PCOSYN) ~.,
                       data = PCOS_data[train_index, c("PCOSYN", x, chosen_predictors)],
                       ntree = 300,
                       importance = TRUE)
    #will use precision to select best one
    tp <- rf$confusion[2,2]
    fp <- rf$confusion[1,2]
    precision <- tp/(fp + tp)
    precision <- ifelse(!is.nan(precision), precision, 0)
    
    if(is.null(max_precision) || max_precision < precision)
    {
      max_precision <- precision
      max_forest <- rf
      max_predictors <- x
    }
    
  }
  chosen_predictors <- append(chosen_predictors, max_predictors)
  chosen_models <- append(chosen_models, list(max_forest))
  available_predictors <- available_predictors[!available_predictors %in% chosen_predictors]
  print(chosen_predictors)
}

#perform accuracy and AUC on training and validation
training_accuracy_list <- c()
validation_accuracy_list <- c()
training_auc_list <- c()
validation_auc_list <- c()

for(p in 1:length(chosen_models))
{
  training_predictions <- as.numeric(predict(chosen_models[[p]], PCOS_data[train_index,])) -1
  print(unique(training_predictions))
  training_accuracy <- mean(ifelse(training_predictions == PCOS_data[train_index,]$PCOSYN, 1, 0))
  training_auc <- auc(roc(PCOS_data[train_index,]$PCOSYN, training_predictions))
  
  validation_predictions <- as.numeric(predict(chosen_models[[p]], PCOS_data[validation_index,])) -1
  validation_accuracy <- mean(ifelse(validation_predictions == PCOS_data[validation_index,]$PCOSYN, 1, 0))
  validation_auc <- auc(roc(PCOS_data[validation_index,]$PCOSYN, validation_predictions))
  
  training_accuracy_list <- append(training_accuracy_list, training_accuracy)
  training_auc_list <- append(training_auc_list, training_auc)
  
  validation_accuracy_list <- append(validation_accuracy_list, validation_accuracy)
  validation_auc_list <- append(validation_auc_list, validation_auc)
}

#plot classification accuracy of training and validation
plot(
  x = 1:length(chosen_models),
  y = training_accuracy_list,
  xlab = "Number of predictors",
  ylab = "Classification Accuracy",
  main = "Accuracy of Random Forest",
  type = "l",
  col = "red"
)
points(
  x = 1:length(chosen_models),
  y = training_accuracy_list,
  col = "red"
)

lines(
  x = 1:length(chosen_models),
  y = validation_accuracy_list,
  col = "blue"
)
points(
  x = 1:length(chosen_models),
  y = validation_accuracy_list,
  col = "blue"
)

legend("topleft",
       legend = c("Training", "Validation"),
       fill = c("red", "blue"))

#plot AUC of training and validation
plot(
  x = 1:length(chosen_models),
  y = training_auc_list,
  xlab = "Number of predictors",
  ylab = "AUC",
  main = "AUC of Random Forest",
  type = "l",
  col = "red"
)
points(
  x = 1:length(chosen_models),
  y = training_auc_list,
  col = "red"
)

lines(
  x = 1:length(chosen_models),
  y = validation_auc_list,
  col = "blue"
)
points(
  x = 1:length(chosen_models),
  y = validation_auc_list,
  col = "blue"
)

legend("topleft",
       legend = c("Training", "Validation"),
       fill = c("red", "blue"))
