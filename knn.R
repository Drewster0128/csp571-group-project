library(class)
library(caret)
library(pROC)
#perform forward selection
available_predictors <- colnames(PCOS_data)[!colnames(PCOS_data) %in% c("PCOS (Y/N)")]
chosen_predictors <- c()
chosen_knn <- c()
for(i in 1:(ncol(PCOS_data) - 1))
{
  max_recall_accuracy <- NULL
  max_predictors <- NULL
  max_knn <- NULL
  for(k in 1:100)
  {
    for(x in available_predictors)
    {
      k_predictions <- knn(
        train = train_data[c(x, chosen_predictors)],
        test = validation_data[c(x, chosen_predictors)],
        cl = as.factor(train_data$`PCOS (Y/N)`),
        k = k
      )
      #calculate recall + accuracy
      cm <- confusionMatrix(k_predictions, as.factor(validation_data$`PCOS (Y/N)`))
      tp <- cm$table[2,2]
      fn <- cm$table[1,2]
      recall <- tp/(tp + fn)
      recall <- ifelse(!is.nan(recall), recall, 0)
      
      accuracy <- (tp + cm$table[1,1])/sum(cm$table)

      recall_accuracy <- accuracy + recall
      
      if(is.null(max_recall_accuracy) || max_recall_accuracy < recall_accuracy)
      {
        max_recall_accuracy <- recall_accuracy
        max_predictors <- x
        max_knn <- c(k =k,pred = list(k_predictions), coef = c(chosen_predictors, max_predictors))
      }
    }
  }
  chosen_predictors <- append(chosen_predictors, max_predictors)
  print(chosen_predictors)
  chosen_knn <- append(chosen_knn, list(max_knn))
  available_predictors <- available_predictors[!available_predictors %in% chosen_predictors]
}

accuracy_list <- c()
auc_list <- c()
best_k <- c()
recall_list <- c()
#get accuracy and AUC of knn on validation set
for(p in 1:length(chosen_knn))
{
  predictions <- as.numeric(chosen_knn[[p]]$pred) -1
  accuracy <- mean(ifelse(validation_data$`PCOS (Y/N)` == predictions, 1, 0))
  auc <- auc(roc(validation_data$`PCOS (Y/N)`, predictions))
  
  cm <- confusionMatrix(k_predictions, as.factor(validation_data$`PCOS (Y/N)`))
  tp <- cm$table[2,2]
  fn <- cm$table[1,2]
  recall <- tp/(tp + fn)
  recall <- ifelse(!is.nan(recall), recall, 0)
  
  accuracy_list <- append(accuracy_list, accuracy)
  auc_list <- append(auc_list, auc)
  best_k <- append(best_k, chosen_knn[[p]]$k)
  recall_list <- append(recall_list, recall)
}

par(mfrow = c(1,1))

#plot accuracy
plot(
  x = 1:length(chosen_knn),
  y = accuracy_list,
  xlab = "Number of predictors",
  ylab = "Classification Accuracy",
  main = "Accuracy of KNN",
  type = "l",
  col = "blue"
)
points(
  x = 1:length(chosen_knn),
  y = accuracy_list,
  col = "blue"
)

#plot AUC
plot(x = 1:length(chosen_knn),
     y = auc_list,
     xlab = "Number of predictors",
     ylab = "AUC",
     main = "AUC of KNN",
     type = "l",
     col = "blue")
points(
  x = 1:length(chosen_knn),
  y = auc_list,
  col = "blue"
)

#plot recall
plot(x = 1:length(chosen_knn),
     y = recall_list,
     xlab = "Number of predictors",
     ylab = "Recall",
     main = "Recall for KNN",
     type = "l",
     col = "blue")
points(
  x = 1:length(chosen_knn),
  y = recall_list,
  col = "blue"
)

#plot best k
plot(x = 1:length(chosen_knn),
     y = best_k,
     xlab = "Number of predictors",
     ylab = "Best Number of Neighbors",
     main = "Best Number of Neighbors for KNN",
     type = "l",
     col = "blue")
points(
  x = 1:length(chosen_knn),
  y = best_k,
  col = "blue"
)

#pick best knn model
#score based on accuracy + recall
best_knn_score <- NULL
best_knn_model <- NULL

for(p in 1:length(chosen_knn))
{
  score <- accuracy_list[p] + recall_list[p]
  
  if(is.null(best_knn_score) || score > best_knn_score)
  {
    best_knn_score <- score
    best_knn_model <- chosen_knn[[p]]
  }
}