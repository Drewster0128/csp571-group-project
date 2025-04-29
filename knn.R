library(class)
library(caret)
library(pROC)
#perform forward selection
available_predictors <- colnames(PCOS_data)[!colnames(PCOS_data) %in% c("PCOS (Y/N)")]
chosen_predictors <- c()
chosen_knn <- c()
for(i in 1:(ncol(PCOS_data) - 1))
{
  max_precision <- NULL
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
      #calculate precision
      cm <- confusionMatrix(k_predictions, as.factor(validation_data$`PCOS (Y/N)`))
      tp <- cm$table[2,2]
      fp <- cm$table[2,1]
      precision <- tp/(tp + fp)
      precision <- ifelse(!is.nan(precision), precision, 0)
      
      if(is.null(max_precision) || max_precision < precision)
      {
        max_precision <- precision
        max_predictors <- x
        max_knn <- c(k =k,pred = list(k_predictions))
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
#get accuracy and AUC of knn on validation set
for(p in 1:length(chosen_knn))
{
  predictions <- as.numeric(chosen_knn[[p]]$pred) -1
  accuracy <- mean(ifelse(validation_data$`PCOS (Y/N)` == predictions, 1, 0))
  auc <- auc(roc(validation_data$`PCOS (Y/N)`, predictions))
  
  accuracy_list <- append(accuracy_list, accuracy)
  auc_list <- append(auc_list, auc)
  best_k <- append(best_k, chosen_knn[[p]]$k)
}

#plot accuracy
plot(
  x = 1:length(chosen_knn),
  y = accuracy_list,
  xlab = "Number of predictors",
  ylab = "Classification Accuracy",
  main = "Accuracy of KNN",
  type = "l",
  col = "red"
)
points(
  x = 1:length(chosen_knn),
  y = accuracy_list,
  col = "red"
)

#plot AUC
plot(x = 1:length(chosen_knn),
     y = auc_list,
     xlab = "Number of predictors",
     ylab = "AUC",
     main = "AUC of KNN",
     type = "l",
     col = "red")
points(
  x = 1:length(chosen_knn),
  y = auc_list,
  col = "red"
)

#plot best k
plot(x = 1:length(chosen_knn),
     y = best_k,
     xlab = "Number of predictors",
     ylab = "Best Number of Neighbors",
     main = "Best Number of Neighbors for KNN",
     type = "l",
     col = "red")
points(
  x = 1:length(chosen_knn),
  y = best_k,
  col = "red"
)