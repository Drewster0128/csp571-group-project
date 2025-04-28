library(tidyverse)
library(readxl)

PCOS_data <- read_xlsx("PCOS_data/PCOS_data_without_infertility.xlsx",
                       sheet = "Full_new")

PCOS_data <- as.data.frame(PCOS_data)

#split into demographic, clinical, and lifestyle attributes
demographic_features <- c("Age (yrs)", "Weight (Kg)", "Height(Cm)", "BMI", "Blood Group", "Hip(inch)", "Waist(inch)", "Waist:Hip Ratio", "Pimples(Y/N)", "Endometrium (mm)")
clinical_features <- c("Pulse rate(bpm)", "RR (breaths/min)", "Hb(g/dl)", "Cycle(R/I)", "Cycle length(days)", "RBS(mg/dl)", "BP _Systolic (mmHg)", "BP _Diastolic (mmHg)", "Follicle No. (L)", "Follicle No. (R)", "Avg. F size (L) (mm)", "Avg. F size (R) (mm)", "TSH (mIU/L)", "AMH(ng/mL)", "PRL(ng/mL)", "Vit D3 (ng/mL)","I   beta-HCG(mIU/mL)", "FSH(mIU/mL)", "LH(mIU/mL)", "FSH/LH", "PRG(ng/mL)", "Weight gain(Y/N)", "hair growth(Y/N)", "Skin darkening (Y/N)", "Hair loss(Y/N)")
lifestyle_features <- c("Marraige Status (Yrs)", "Fast food (Y/N)", "Reg.Exercise(Y/N)", "Pregnant(Y/N)", "No. of aborptions")

#drop some unnecessary features
uncessary_features <- c("Sl. No", "Patient File No.", "II    beta-HCG(mIU/mL)", "...45")
PCOS_data <- PCOS_data[, !names(PCOS_data) %in% uncessary_features]

#one-hot encoding to categorical features
#categorical data: PCOS (Y/N), Blood Group, Pregnant(Y/N), Weight gain(Y/N), hair growth(Y/N), Skin darkening (Y/N), Hair loss(Y/N), Pimples(Y/N), Fast food (Y/N), Reg.Exercise(Y/N)
categorical_features <- c("PCOS (Y/N)", "Blood Group", "Pregnant(Y/N)", "Weight gain(Y/N)", "hair growth(Y/N)", "Skin darkening (Y/N)", "Hair loss(Y/N)", "Pimples(Y/N)", "Fast food (Y/N)", "Reg.Exercise(Y/N)")

#convert to factors
for(feature in categorical_features)
{
  PCOS_data[, feature] <- as.factor(PCOS_data[, feature])
}

#one-hot encoding
one_hot_data <- model.matrix(~ . - 1, PCOS_data[categorical_features])
print(PCOS_data[categorical_features])
