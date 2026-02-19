# Installing and loading packages
install.packages(c("dplyr", "openxlsx"))
library(dplyr)
library(openxlsx)

# Generating dataset for the drug survey

set.seed(2025)

n_drug <- 200

drug_data <- data.frame(
  ID = sprintf("Q%03dd", 1:n_drug),
  Incentive_Offered = sample(c("$10 gift card","Movie ticket","Online voucher"), n_drug, replace = TRUE),
  Age_Group = sample(c("14-17","18-21","22-25","26-29","30-33","34-37","38-41"), n_drug, replace = TRUE, prob = c(0.2,0.25,0.25,0.15,0.1,0.03,0.02)),
  Gender = sample(c("Male","Female","Non-binary/Third gender","Prefer not to say"), n_drug, replace = TRUE, prob = c(0.45,0.45,0.05,0.05)),
  Family_Relationship = sample(c("Very close","Good","Sometimes difficult","Distant","No contact"), n_drug, replace = TRUE, prob = c(0.3,0.3,0.2,0.15,0.05)),
  Peer_Pressure = sample(c("Strong pressure","Some pressure","Neutral","Pressure not to use","Not applicable"), n_drug, replace = TRUE),
  Economic_Status = sample(c("Comfortable","Getting by","Struggling","Prefer not to say"), n_drug, replace = TRUE, prob = c(0.25,0.4,0.25,0.1)),
  Drug_Exposure = sample(c("Very common","Common","Some","Few","None"), n_drug, replace = TRUE),
  Reason_First_Try = sample(c("Curiosity","Peer pressure","Stress","Escape","Boredom","Mental health","Family issues"), n_drug, replace = TRUE),
  Want_Help = sample(c("Yes","No","Not sure"), n_drug, replace = TRUE, prob = c(0.6,0.25,0.15)),
  Prevention_Preference = sample(c("Education","Community programs","Mental health support","Family support","Recreation","Peer mentoring"), n_drug, replace = TRUE),
  Moral_View = sample(c("Always wrong","Generally wrong","Personal choice","Acceptable sometimes","Acceptable"), n_drug, replace = TRUE),
  Importance_Family = sample(1:5, n_drug, replace = TRUE, prob = rev(1:5)),
  Importance_Health = sample(1:5, n_drug, replace = TRUE, prob = rev(1:5)),
  Importance_Friends = sample(1:5, n_drug, replace = TRUE),
  Accessibility = sample(c("Very easy","Somewhat easy","Moderate","Difficult","Unknown"), n_drug, replace = TRUE),
  Comment = sample(c("Need more awareness in schools","Peer pressure is the main issue","Better community support needed","Media glamorization is harmful",""), n_drug, replace = TRUE, prob = c(0.2,0.2,0.2,0.2,0.2))
)

head(drug_data)
str(drug_data)
dim(drug_data)
