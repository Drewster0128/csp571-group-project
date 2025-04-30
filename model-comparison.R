library(class)
library(pROC)
#run models on test dataset for best_knn_model, best_logistic_modle, and best_rf_model

#logistic regression predictions
log_test_predictions <- predict(best_logistic_model, test_data, type = "response")
log_test_predictions <- ifelse(log_test_predictions >= 0.50, 1, 0)

#knn predictions
knn_test_predictions <- knn(train = train_data[, c(best_knn_model$coef1, best_knn_model$coef2, best_knn_model$coef3, best_knn_model$coef4)],
                            test = test_data[, c(best_knn_model$coef1, best_knn_model$coef2, best_knn_model$coef3, best_knn_model$coef4)],
                            cl = train_data$`PCOS (Y/N)`,
                            k = best_knn_model$k)
knn_test_predictions <- as.numeric(knn_test_predictions) - 1

#rf predictions
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

rf_test_predictions <- predict(best_rf_model, PCOS_data[test_index, ])
rf_test_predictions <- as.numeric(rf_test_predictions) - 1

#accuracy
log_test_accuracy <- mean(ifelse(log_test_predictions == test_data$`PCOS (Y/N)`, 1, 0))
knn_test_accuracy <- mean(ifelse(knn_test_predictions == test_data$`PCOS (Y/N)`, 1, 0))
rf_test_accuracy <- mean(ifelse(rf_test_predictions == test_data$`PCOS (Y/N)`, 1, 0))

#auc
log_test_auc <- auc(roc(test_data$`PCOS (Y/N)`, log_test_predictions))
knn_test_auc <- auc(roc(test_data$`PCOS (Y/N)`, knn_test_predictions))
rf_test_auc <- auc(roc(test_data$`PCOS (Y/N)`, rf_test_predictions))

tp <- function(predictions, actual) {
  return(sum(sapply(1:length(predictions), function(i) {
    if(predictions[i] == 1 && actual[i] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  })))
}

fp <- function(predictions, actual) {
  return(sum(sapply(1:length(predictions), function(i) {
    if(predictions[i] == 1 && actual[i] == 0)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  })))
}

fn <- function(predictions, actual) {
  return(sum(sapply(1:length(predictions), function(i) {
    if(predictions[i] == 0 && actual[i] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  })))
}

#tp
rf_test_tp <- tp(rf_test_predictions, test_data$`PCOS (Y/N)`)
log_test_tp <- tp(log_test_predictions, test_data$`PCOS (Y/N)`)
knn_test_tp <- tp(knn_test_predictions, test_data$`PCOS (Y/N)`)

#fp
rf_test_fp <- fp(rf_test_predictions, test_data$`PCOS (Y/N)`)
log_test_fp <- fp(log_test_predictions, test_data$`PCOS (Y/N)`)
knn_test_fp <- fp(knn_test_predictions, test_data$`PCOS (Y/N)`)

#fn
rf_test_fn <- fn(rf_test_predictions, test_data$`PCOS (Y/N)`)
log_test_fn <- fn(log_test_predictions, test_data$`PCOS (Y/N)`)
knn_test_fn <- fn(knn_test_predictions, test_data$`PCOS (Y/N)`)

#precision
rf_test_precision <- rf_test_tp / (rf_test_tp + rf_test_fp)
log_test_precision <- log_test_tp / (log_test_tp + log_test_fp)
knn_test_precision <- knn_test_tp / (knn_test_tp + knn_test_fp)

#recall
rf_test_recall <- rf_test_tp / (rf_test_tp + rf_test_fn)
log_test_recall <- log_test_tp / (log_test_tp + log_test_fn)
knn_test_recall <- knn_test_tp / (knn_test_tp + knn_test_fn)

#f1
rf_test_f1 <- (2 * rf_test_precision * rf_test_recall) / (rf_test_precision + rf_test_recall)
log_test_f1 <- (2 * log_test_precision * log_test_recall) / (log_test_precision + log_test_recall)
knn_test_f1 <- (2 * knn_test_precision * knn_test_recall) / (knn_test_precision + knn_test_recall)


rename_features <- function(features) 
{
  return(sapply(features, function(column_name) {
    column_name <- gsub(" ", "", column_name)
    column_name <- gsub("\\(", "", column_name)
    column_name <- gsub("\\)", "", column_name)
    column_name <- gsub("\\/", "", column_name)
    column_name <- gsub("\\-", "", column_name)
    column_name <- gsub("\\:", "", column_name)
    column_name <- gsub("\\`", "", column_name)
    column_name <- gsub("\\\\", "", column_name)
    return(column_name)
  }))
}

#histogram of features
best_knn_model_features <- rename_features(c(sprintf("%s", best_knn_model$coef1), best_knn_model$coef2, best_knn_model$coef3, best_knn_model$coef4))
best_log_model_features <- rename_features(attr(best_logistic_model$coefficients, "names")[-1])
best_rf_model_features <- rename_features(attr(best_rf_model$importance, "dimnames")[[1]])

features <- features[order(c(best_knn_model_features, best_log_model_features, best_rf_model_features))]
features_unique <- colnames(PCOS_data[, -1])
features_unique <- features_unique[order(features_unique)]
feature_frequency <- sapply(features_unique, function(feature) {
  return(sum(ifelse(features == feature, 1, 0)))
})

demographic_features <- rename_features(colnames(demographic_data))
clinical_features <- rename_features(colnames(clinical_data))
lifestyle_features <- rename_features(colnames(lifestyle_data))


par(mar=c(10,6,4,1))
barplot(
  feature_frequency,
  names.arg = features_unique,
  ylab = "Number of models",
  main = "Frequency of Predictors used in the Three Models",
  col = sapply(features_unique, function(feature) {
    if(feature %in% demographic_features)
    {
      return("blue")
    }
    else if(feature %in% clinical_features)
    {
      return("red")
    }
    else
    {
      return("yellow")
    }
  }),
  las = 2
)

legend('topleft', c("Demographic Feature", "Clinical Feature", "Lifestyle Feature"), fill=c("blue", "red", "yellow"))

#plot bar-graph of bestmodel accuracy
par()
accuracy_plot <- barplot(
  c(log_test_accuracy, rf_test_accuracy, knn_test_accuracy),
  names.arg = c("Logistic Regression", "Random Forest", "KNN"),
  xlab = "Model Type",
  ylab = "Test Accuracy",
  main = "Test Accuracy on Models",
)
text(accuracy_plot, 0, round(c(log_test_accuracy, rf_test_accuracy, knn_test_accuracy), 2),cex=1,pos=3)

#plot bar-graph of best model AUC
auc_list <- c(log_test_auc, rf_test_auc, knn_test_auc)
auc_plot <- barplot(
  auc_list,
  names.arg = c("Logistic Regression", "Random Forest", "KNN"),
  xlab = "Model Type",
  ylab = "Test AUC",
  main = "Test AUC on Models",
)
text(auc_plot, 0, round(auc_list, 2),cex=1,pos=3)

#plot bar-graph of best model precision
par()
precision_list <- c(log_test_precision, rf_test_precision, knn_test_precision)
precision_plot <- barplot(
  precision_list,
  names.arg = c("Logistic Regression", "Random Forest", "KNN"),
  xlab = "Model Type",
  ylab = "Test Precision",
  main = "Test Precision on Models"
)
text(precision_plot, 0, round(precision_list, 2), cex = 1, pos = 3)

#plot bar-graph of best model recall
recall_list <- c(log_test_recall, rf_test_recall, knn_test_recall)
recall_plot <- barplot(
  recall_list,
  names.arg = c("Logistic Regression", "Random Forest", "KNN"),
  xlab = "Model Type",
  ylab = "Test Recall",
  main = "Test Recall on Models"
)
text(recall_plot, 0, round(recall_list, 2), cex = 1, pos = 3)

#plot bar-graph of best model f1
f1_list <- c(log_test_f1, rf_test_f1, knn_test_f1)
f1_plot <- barplot(
  f1_list,
  names.arg = c("Logistic Regression", "Random Forest", "KNN"),
  xlab = "Model Type",
  ylab = "Test F1",
  main = "Test F1 on Models"
)
text(f1_plot, 0, round(f1_list, 2), cex = 1, pos = 3)
