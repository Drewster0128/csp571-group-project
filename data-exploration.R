library(ggplot2)
#make sure to fun data-preparation first

#data visualization on demographic data
demographic_features <- c("Age (yrs)", "Weight (Kg)", "Height(Cm)", "BMI", "`Blood Group`11", "`Blood Group`12", "`Blood Group`13", "`Blood Group`14", "`Blood Group`15", "`Blood Group`16", "`Blood Group`17", "`Blood Group`18", "Hip(inch)", "Waist(inch)", "Waist:Hip Ratio", "`Pimples(Y/N)`1", "Endometrium (mm)")
demographic_data <- PCOS_data[, demographic_features]
Y <- PCOS_data$`PCOS (Y/N)`

for(feature in demographic_features)
{
  if(feature %in% categorical_features)
  {
    yes_has_pcos_freq <- nrow(PCOS_data[(PCOS_data$`PCOS (Y/N)` == 1 & PCOS_data[, feature] == 1),])
    no_has_pcos_freq <- nrow(PCOS_data[(PCOS_data$`PCOS (Y/N)` == 1 & PCOS_data[, feature] == 0),])
    
    yes_no_pcos_freq <- nrow(PCOS_data[(PCOS_data$`PCOS (Y/N)` == 0 & PCOS_data[, feature] == 1),])
    no_no_pcos_freq <- nrow(PCOS_data[(PCOS_data$`PCOS (Y/N)` == 0 & PCOS_data[, feature] == 0),])
    
    values <- matrix(c(yes_has_pcos_freq, no_has_pcos_freq, yes_no_pcos_freq, no_no_pcos_freq), ncol = 2, byrow = TRUE)
    
    barplot(values, col = c("red", "blue"), names.arg = c("Yes", "No"), xlab = feature, ylab = "Frequency", main = sprintf("Distribution of observations with %s", feature))
    legend('topleft', c("Has PCOS", "Doesn't have PCOS"), fill=c("red", "blue"))
  }
  else
  {
    plot(x = as.factor(Y),
         y = PCOS_data[, feature],
         xlab = "PCOS",
         ylab = feature,
         col = c("blue", "red"),
         main = sprintf("Box Plot of %s", feature))
    hist(PCOS_data[, feature],
         xlab = feature,
         ylab = "Frequency",
         col = "blue",
         main = sprintf("Histogram of %s", feature))
  }
}