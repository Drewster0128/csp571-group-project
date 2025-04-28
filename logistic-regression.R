#make sure you've run train-test-split before running this
library(leaps)
library(bestglm)

#foward selection on PCOS_data for logistic regression
#we will use AIC as the chosen metric
available_predictors <- colnames(PCOS_data)[!colnames(PCOS_data) %in% c("PCOS (Y/N)")]
chosen_predictors <- c()
for(i in 1:(ncol(PCOS_data) -1))
{
  #take all possible combinations of the available predictors
  combinations <- combn(available_predictors, i)
  print(object.size(combinations)) #this guy is taking a large amount of space
  for(j in ncol(combinations))
  {
    model_data <- train_data[, c("PCOS (Y/N)", combinations[, j], chosen_predictors)]
    #fit model
    model <- glm(`PCOS (Y/N)` ~ .,
                 family = binomial,
                 data = model_data)
  }
}


