library(tidyverse)
library(readxl)

PCOS_data <- read_xlsx("PCOS_data/PCOS_data_without_infertility.xlsx",
                       sheet = "Full_new")

#split into demographic, clinical, and lifestyle attributes
demographic_features <- c("Age (yrs)", "Weight (Kg)", "Height(Cm)", "BMI", "Blood Group", "Hip(inch)", "Waist(inch)", "Waist:Hip Ratio", "")
clinical_features <- c("Pulse rate(bpm)", "RR (breaths/min)", "Hb(g/dl)", "Cycle(R/I)", "Cycle length(days)", "RBS(mg/dl)")
lifestyle_features <- c("Marriage Status (Yrs)", "Fast food (Y/N)", "Reg.Exercise(Y/N)")