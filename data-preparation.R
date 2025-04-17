library(tidyverse)
library(readxl)

PCOS_data <- read_xlsx("PCOS_data/PCOS_data_without_infertility.xlsx",
                       sheet = "Full_new")

#split into demographic, clinical, and lifestyle attributes
demographic_features <- c("Age (yrs)", "Weight (Kg)", "Height(Cm)", "BMI", "Blood Group", "Hip(inch)", "Waist(inch)", "Waist:Hip Ratio", "Pimples(Y/N)", "Endometrium (mm)")
clinical_features <- c("Pulse rate(bpm)", "RR (breaths/min)", "Hb(g/dl)", "Cycle(R/I)", "Cycle length(days)", "RBS(mg/dl)", "BP _Systolic (mmHg)", "BP _Diastolic (mmHg)", "Follicle No. (L)", "Follicle No. (R)", "Avg. F Size (L) (mm)", "Avg. F Size (R) (mm)", "TSH (mIU/L)", "AMH(ng/mL)", "PRL(ng/mL)", "Vit D3 (ng/mL)","I   beta-HCG(mIU/mL)", "FSH(mIU/mL)", "LH(mIU/mL)", "FSH/LH")
lifestyle_features <- c("Marriage Status (Yrs)", "Fast food (Y/N)", "Reg.Exercise(Y/N)", "Pregant(Y/N)", "No. of aborptions")

#drop some unnecessary features
uncessary_features <- c("Sl. No", "Patient File No.", "II    beta-HCG(mIU/mL)", "...45")
PCOS_data <- PCOS_data[, !names(PCOS_data) %in% uncessary_features]