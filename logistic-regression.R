#make sure you've run train-test-split before running this
library(leaps)
library(bestglm)
library(iterators)
library(boot)
library(pROC)

#forward selection on PCOS_data for logistic regression
#we will use the min AIC and precision cost
available_predictors <- colnames(PCOS_data)[!colnames(PCOS_data) %in% c("PCOS (Y/N)")]
chosen_predictors <- c()
chosen_models <- c()
for(i in 1:(ncol(PCOS_data) -1))
{
  min_cost <- NULL
  min_aic <- NULL
  min_model <- NULL
  min_predictors <- NULL
  for(x in available_predictors)
  {
    fitted_model <- glm(`PCOS (Y/N)` ~ ., family = binomial, data = train_data[, c("PCOS (Y/N)", x, chosen_predictors)])
    predictions <- predict(fitted_model, train_data, type = "response")
    predictions <- ifelse(predictions >= 0.50, 1, 0)
    tp <- sum(sapply(1:nrow(train_data), function(p) {
      if(predictions[p] == 1 && train_data$`PCOS (Y/N)`[p] == 1)
      {
        return(1)
      }
      else
      {
        return(0)
      }
    }))
    fn <- sum(sapply(1:nrow(train_data), function(p) {
      if(predictions[p] == 0 && train_data$`PCOS (Y/N)`[p] == 1)
      {
        return(1)
      }
      else
      {
        return(0)
      }
    }))
    recall <- tp / (tp + fn)
    recall <- ifelse(!is.nan(recall), recall, 0)
    
    cost <- (fitted_model$aic) + (1 - recall)
    if(is.null(min_cost) || cost < min_cost)
    {
      min_cost <- cost
      min_aic <- fitted_model$aic
      min_model <- fitted_model
      min_predictors <- x
    }
  }
  chosen_predictors <- append(chosen_predictors, min_predictors)
  chosen_models <- append(chosen_models, list(min_model))
  available_predictors <- available_predictors[!available_predictors %in% chosen_predictors]
}
par(mfrow= c(1,1))
#amongst 47 models, get training accuracy, validation accuracy, and AIC
training_accuracy_list <- c()
validation_accuracy_list <- c()
aic_list <- c()
training_auc_list <- c()
validation_auc_list <- c()
training_recall_list <- c()
validation_recall_list <- c()
for(p in 1:length(chosen_models))
{
  training_predictions <- predict(chosen_models[[p]], newdata = train_data[, -1], type="response")
  training_predictions <- ifelse(training_predictions >= 0.5, 1, 0)
  training_accuracy <- mean(ifelse(train_data$`PCOS (Y/N)` == training_predictions, 1, 0))
  training_accuracy_list <- append(training_accuracy_list, training_accuracy)
  
  validation_predictions <- predict(chosen_models[[p]], newdata = validation_data[, -1], type="response")
  validation_predictions <- ifelse(validation_predictions >= 0.5, 1, 0)
  validation_accuracy <- mean(ifelse(validation_data$`PCOS (Y/N)` == validation_predictions, 1, 0))
  validation_accuracy_list <- append(validation_accuracy_list, validation_accuracy)
  
  aic_list <- append(aic_list, chosen_models[[p]]$aic)
  
  training_area_under_curve <- auc(roc(train_data$`PCOS (Y/N)`, training_predictions))
  training_auc_list <- append(training_auc_list, training_area_under_curve)
  
  validation_area_under_curve <- auc(roc(validation_data$`PCOS (Y/N)`, validation_predictions))
  validation_auc_list <- append(validation_auc_list, validation_area_under_curve)
  
  training_tp <- sum(sapply(1:length(training_predictions), function(p) {
    if(training_predictions[p] == 1 && train_data$`PCOS (Y/N)`[p] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }))
  training_fn <- sum(sapply(1:length(training_predictions), function(p) {
    if(training_predictions[p] == 0 && train_data$`PCOS (Y/N)`[p] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }))
  training_recall <- training_tp / (training_tp + training_fn)
  training_recall <- ifelse(!is.nan(training_recall), training_recall, 0)
  
  validation_tp <- sum(sapply(1:length(validation_predictions), function(p) {
    if(validation_predictions[p] == 1 && validation_data$`PCOS (Y/N)`[p] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }))
  
  validation_fn <- sum(sapply(1:length(validation_predictions), function(p) {
    if(validation_predictions[p] == 0 && validation_data$`PCOS (Y/N)`[p] == 1)
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }))
  
  validation_recall <- validation_tp / (validation_tp + validation_fn)
  validation_recall <- ifelse(!is.nan(validation_recall), validation_recall, 0)
  
  training_recall_list <- append(training_recall_list, training_recall)
  validation_recall_list <- append(validation_recall_list, validation_recall)
}

#calculate AUC

#plot training accuracy and validation accuracy with number of predictors as the x axis

#line for training accuracy
plot(
  x = 1:length(chosen_models),
  y = training_accuracy_list,
  xlab = "Number of predictors",
  ylab = "Classification Accuracy",
  main = "Forward Selection on Logistic Regression",
  type = "l",
  col = "red"
)

#points for training accuracy
points(
  x = 1:length(chosen_models),
  y = training_accuracy_list,
  col = "red"
)

#line for validation accuracy
lines(
  x = 1:length(chosen_models),
  y = validation_accuracy_list,
  col = "blue"
)

#points for validation accuracy
points(
  x = 1:length(chosen_models),
  y = validation_accuracy_list,
  col = "blue"
)

#add legend
legend("topleft",
       legend = c("Training", "Validation"),
       fill = c("red", "blue"))

#plot for AUC
plot(
  x = 1:length(chosen_models),
  y = training_auc_list,
  col = "red",
  xlab = "Number of predictors",
  ylab = "AUC",
  main = "AUC Forward Selection on Logistic Regression",
  type = "l"
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



#plot for AIC
plot(
  x = 1:length(chosen_models),
  y = sapply(1:length(chosen_models), function(p) chosen_models[[p]]$aic),
  xlab = "Number of predictors",
  ylab = "AIC",
  main = "AIC on Forward Selection for Logistic Regression",
  type = "l"
)
points(
  x = 1:length(chosen_models),
  y = sapply(1:length(chosen_models), function(p) chosen_models[[p]]$aic)
)

#plot for recall
plot(x = 1:length(chosen_models),
     y = training_recall_list,
     xlab = "Number of predictors",
     ylab = "Recall",
     main = "Recall on Logistic Regression",
     type = "l",
     col = "red")
points(x = 1:length(chosen_models),
       y = training_recall_list,
       col = "red")

lines(x = 1:length(chosen_models),
      y = validation_recall_list,
      col = "blue")
points(x = 1:length(chosen_models),
       y = validation_recall_list,
       col = "blue")

classification_cost<- function(observed, pred.p) {
  predictions <- ifelse(pred.p >= 0.5, 1, 0)
  accuracy <- mean(ifelse(observed == predictions, 1, 0))
  cost <- 1 - accuracy
  return(cost)
}

auc_cost <- function(observed, pred.p) {
  predictions <- ifelse(pred.p >= 0.5, 1, 0)
  area_under_curve <- auc(roc(observed, predictions))
  return(1 - area_under_curve)
}

#assess with 10 fold cross validation on accuracy
cv_err_list <- sapply(1:length(chosen_models), function(p) {
  cv.glm(train_data, chosen_models[[p]], K = 10, cost = classification_cost)$delta[1]
})


#plot cv-error for each number of predictors
plot(
  x = 1:length(chosen_models),
  y = 1 - cv_err_list,
  xlab = "Number of predictors",
  ylab = "CV Accuracy",
  main = "10-fold Cross Validation Accuracy for Forward Selection Logistic Regression",
  type = "l"
)
points(
  x = 1:length(chosen_models),
  y = 1 - cv_err_list
)

#assess with 10 fold cross validation on AUC
cv_auc_cost_list <- sapply(1:length(chosen_models), function(p) {
  cv.glm(train_data, chosen_models[[p]], K = 10, cost = auc_cost)$delta[1]
})

#plot cv-auc for each number of predictors
plot(
  x = 1:length(chosen_models),
  y = 1 - cv_auc_cost_list,
  xlab = "Number of predictors",
  ylab = "CV AUC",
  main = "10-fold CV AUC for Forward Selection on Logisitc Regression",
  type = "l"
)
points(
  x = 1:length(chosen_models),
  y = 1 - cv_auc_cost_list
)

#pick best model
#use -AIC, recall, and accuracy summation to determine best model
best_logistic_score <- NULL
best_logistic_model <- NULL
for(p in 1:length(chosen_models))
{
  score <- validation_recall_list[p] + validation_accuracy_list[p] - aic_list[p]
  if(is.null(best_logistic_score) || score > best_logistic_score)
  {
    best_logistic_score <- score
    best_logistic_model <- chosen_models[[p]]
  }
}

