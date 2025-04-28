#make sure you've run train-test-split before running this
library(leaps)
library(bestglm)
library(iterators)

#foward selection on PCOS_data for logistic regression
#we will use AIC as the chosen metric
available_predictors <- colnames(PCOS_data)[!colnames(PCOS_data) %in% c("PCOS (Y/N)")]
chosen_predictors <- c()
for(i in 1:(ncol(PCOS_data) -1))
{
  min_aic <- NULL
  min_model <- NULL
  min_predictors <- NULL
  for(x in available_predictors)
  {
    fitted_model <- glm(`PCOS (Y/N)` ~ ., family = binomial, data = train_data[, c("PCOS (Y/N)", x, chosen_predictors)])
    if(is.null(min_aic) || fitted_model$aic < min_aic)
    {
      min_aic <- fitted_model$aic
      min_model <- fitted_model
      min_predictors <- x
    }
  }
  chosen_predictors <- append(chosen_predictors, min_predictors)
  print(chosen_predictors)
  available_predictors <- available_predictors[!available_predictors %in% chosen_predictors]
}


