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
  max_recall <- NULL
  max_predictors <- NULL
  max_forest <- NULL
  for(x in available_predictors)
  {
    rf <- randomForest(as.factor(PCOSYN) ~.,
                       data = PCOS_data[train_index, c("PCOSYN", x, chosen_predictors)],
                       ntree = 300,
                       importance = TRUE)
    #will use recall to select best one
    tp <- rf$confusion[2,2]
    fn <- rf$confusion[2,1]
    recall <- tp/(fn + tp)
    recall <- ifelse(!is.nan(recall), recall, 0)
    
    if(is.null(max_recall) || max_recall < recall)
    {
      max_recall <- recall
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
training_recall_list <- c()
validation_recall_list <- c()

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
  
  training_tp <- chosen_models[[p]]$confusion[2,2]
  training_fn <- chosen_models[[p]]$confusion[2,1]
  training_recall <- training_tp / (training_tp + training_fn)
  training_recall_list <- append(training_recall_list, training_recall)
  
  validation_tp <- sum(sapply(1:length(validation_predictions), function(i) {
    if(validation_predictions[i] == 1 && PCOS_data[validation_index,]$PCOSYN[i] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }))
  validation_fn <- sum(sapply(1:length(validation_predictions), function(i) {
    if(validation_predictions[i] == 0 && PCOS_data[validation_index,]$PCOSYN[i] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }))
  validation_recall <- validation_tp / (validation_tp + validation_fn)
  validation_recall_list <- append(validation_recall_list, validation_recall)
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

#plot recall on training and validation
plot(
  x = 1:length(chosen_models),
  y = training_recall_list,
  xlab = "Number of predictors",
  ylab = "Recall",
  main = "Recall on Random Forest",
  col = "red",
  type = "l"
)
points(
  x = 1:length(chosen_models),
  y = training_recall_list,
  col = "red"
)

lines(
  x = 1:length(chosen_models),
  y = validation_recall_list,
  col = "blue"
)
points(
  x = 1:length(chosen_models),
  y = validation_recall_list,
  col = "blue"
)

legend("topleft",
       legend = c("Training", "Validation"),
       fill = c("red", "blue"))

#choose best random forest model
#scored by accuracy + recall
best_rf_score <- NULL
best_rf_model <- NULL
for(p in 1:length(chosen_models))
{
  score <- validation_accuracy_list[p] + validation_recall_list[p]
  print(score)
  
  if(is.null(best_rf_score) || score > best_rf_score)
  {
    best_rf_score <- score
    best_rf_model <- chosen_models[[p]]
  }
}