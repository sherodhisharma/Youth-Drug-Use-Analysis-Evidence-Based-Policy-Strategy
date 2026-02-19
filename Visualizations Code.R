# =============================================
# R Script: Travel & Drug Survey Dataset
# =============================================

# ---------------------------
# Packages
# ---------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(ggplot2)
# ---------------------------
# Helper: Multi-select function
# ---------------------------
multi_select <- function(n, options, max_k=3, allow_zero=FALSE){
  sapply(1:n, function(x){
    k <- if(allow_zero) sample(0:max_k, 1) else sample(1:max_k,1)
    if(k==0) return(NA)
    paste(sample(options, k), collapse = "; ")
  })
}

# =============================================
# Part 1: Travel Survey Dataset
# =============================================
n_base_travel <- 20
base_travel <- data.frame(
  base_id = sprintf("BASE_T%02d", 1:n_base_travel),
  age_group = sample(c("18-21","22-25","26-30","31-35","36-40","41-45"),
                     n_base_travel, replace=TRUE),
  gender = sample(c("Male","Female","Non-binary/Third gender","Prefer not to say"),
                  n_base_travel, replace=TRUE),
  education_level = sample(c("High school","Diploma/Certificate","Undergraduate","Postgraduate"),
                           n_base_travel, replace=TRUE),
  travel_experience_years = sample(0:10, n_base_travel, replace=TRUE)
)

# Sample Travel Dataset
n_travel <- 200
age_props_travel <- prop.table(table(base_travel$age_group))
gender_props_travel <- prop.table(table(base_travel$gender))
edu_props_travel <- prop.table(table(base_travel$education_level))

travel <- data.frame(
  respondent_id = sprintf("T%03d", 1:n_travel),
  age_group = sample(names(age_props_travel), n_travel, replace=TRUE, prob=as.vector(age_props_travel)),
  gender = sample(names(gender_props_travel), n_travel, replace=TRUE, prob=as.vector(gender_props_travel)),
  education_level = sample(names(edu_props_travel), n_travel, replace=TRUE, prob=as.vector(edu_props_travel)),
  travel_experience_years = sample(0:10, n_travel, replace=TRUE)
)

# Numeric conversion for age
convert_age_group_to_numeric <- function(ag) {
  sapply(ag, function(x) {
    if(x=="18-21") round(runif(1,18,21))
    else if(x=="22-25") round(runif(1,22,25))
    else if(x=="26-30") round(runif(1,26,30))
    else if(x=="31-35") round(runif(1,31,35))
    else if(x=="36-40") round(runif(1,36,40))
    else if(x=="41-45") round(runif(1,41,45))
    else NA
  })
}
travel$age_numeric <- convert_age_group_to_numeric(travel$age_group)

# Multi-selects
travel$preferred_destinations <- multi_select(n_travel, c("Beach","Mountain","City","Cruise","Adventure","Historical","Nature"), max_k=3)
travel$travel_modes <- multi_select(n_travel, c("Plane","Train","Bus","Car","Bicycle","Walk"), max_k=2)

# =============================================
# Part 2: Drug Survey Dataset
# =============================================
n_base_drug <- 20
base_drug <- data.frame(
  base_id = sprintf("BASE_D%02d", 1:n_base_drug),
  age_group = sample(c("18-21","22-25","26-30","31-35","36-40","41-45"),
                     n_base_drug, replace=TRUE),
  gender = sample(c("Male","Female","Non-binary/Third gender","Prefer not to say"),
                  n_base_drug, replace=TRUE),
  education_level = sample(c("High school","Diploma/Certificate","Undergraduate","Postgraduate"),
                           n_base_drug, replace=TRUE),
  employment_status = sample(c("Full-time employed","Part-time employed","Student","Self-employed","Unemployed"),
                             n_base_drug, replace=TRUE),
  drug_experience_years = sample(0:10, n_base_drug, replace=TRUE)
)

# Sample Drug Dataset
n_drug <- 200
age_props_drug <- prop.table(table(base_drug$age_group))
gender_props_drug <- prop.table(table(base_drug$gender))
edu_props_drug <- prop.table(table(base_drug$education_level))
emp_props_drug <- prop.table(table(base_drug$employment_status))

drug <- data.frame(
  respondent_id = sprintf("D%03d", 1:n_drug),
  age_group = sample(names(age_props_drug), n_drug, replace=TRUE, prob=as.vector(age_props_drug)),
  gender = sample(names(gender_props_drug), n_drug, replace=TRUE, prob=as.vector(gender_props_drug)),
  education_level = sample(names(edu_props_drug), n_drug, replace=TRUE, prob=as.vector(edu_props_drug)),
  employment_status = sample(names(emp_props_drug), n_drug, replace=TRUE, prob=as.vector(emp_props_drug)),
  drug_experience_years = sample(0:10, n_drug, replace=TRUE)
)

# Numeric conversion
drug$age_numeric <- convert_age_group_to_numeric(drug$age_group)

# Multi-selects
drug$preferred_drugs <- multi_select(n_drug, c("Cannabis","Cocaine","MDMA","LSD","Prescription opioids","Heroin","Methamphetamine","Other"), max_k=3)
drug$consumption_frequency <- multi_select(n_drug, c("Daily","Weekly","Monthly","Occasionally","Experimented once"), max_k=2)
drug$risk_aware <- multi_select(n_drug, c("Read warnings","Followed advice","Sought info online","Asked peers","Ignored guidelines"), max_k=3, allow_zero=TRUE)
drug$support_accessed <- multi_select(n_drug, c("Medical professional","Counselor","Peer group","Hotline","None"), max_k=2, allow_zero=TRUE)

# =============================================
# Part 3: Export datasets
# =============================================
write.xlsx(travel, "Travel_Survey_Data_Complete.xlsx", overwrite=TRUE)
write.xlsx(drug, "Drug_Survey_Data_Complete.xlsx", overwrite=TRUE)

# =============================================
# Part 4: Quick descriptive stats & plots
# =============================================

# Travel Age Histogram
ggplot(travel, aes(x=age_numeric)) +
  geom_histogram(binwidth=2, fill="skyblue", color="black") +
  theme_minimal() + labs(title="Travel Survey: Age Distribution", x="Age", y="Count")

# Drug Experience Histogram
ggplot(drug, aes(x=drug_experience_years)) +
  geom_histogram(binwidth=1, fill="salmon", color="black") +
  theme_minimal() + labs(title="Drug Survey: Experience Years", x="Years of Drug Experience", y="Count")

# Travel Preferred Destinations
dest_freq <- unlist(strsplit(travel$preferred_destinations, "; "))
dest_table <- table(dest_freq)
barplot(dest_table, col="lightgreen", main="Preferred Travel Destinations", las=2)

# Drug Preferred Drugs
drug_freq <- unlist(strsplit(drug$preferred_drugs, "; "))
drug_table <- table(drug_freq)
barplot(drug_table, col="lightpink", main="Preferred Drugs", las=2)

########################################################33
 # Travel Stat Analysis
########################################################3
# Add amount_spent in $ (e.g., $500-$5000)
set.seed(123)
travel$amount_spent <- round(runif(nrow(travel), 500, 5000), 0)

# Create Age Strata
travel$age_strata <- cut(travel$age_numeric, breaks=c(17,25,35,45),
                         labels=c("18-25","26-35","36-45"))

# Create Amount Spent Strata
travel$spend_strata <- cut(travel$amount_spent, breaks=c(0,2000,3500,5000),
                           labels=c("Low","Medium","High"))

# Proportional allocation for sample size per stratum
N <- nrow(travel)
strata_counts <- table(travel$age_strata)
strata_proportion <- strata_counts / N
strata_sample_size <- round(strata_proportion * N)  # n_h for each stratum
strata_sample_size

# Weighted mean & SE for stratified sample
library(dplyr)
age_summary <- travel %>%
  group_by(age_strata) %>%
  summarise(mean_age = mean(age_numeric),
            sd_age = sd(age_numeric),
            n = n())

# Overall weighted mean
overall_mean_age <- sum(age_summary$mean_age * age_summary$n) / sum(age_summary$n)

# Standard error for stratified mean
se_age <- sqrt(sum((age_summary$n/N)^2 * (age_summary$sd_age^2 / age_summary$n)))
ci_age <- overall_mean_age + c(-1,1) * qnorm(0.975) * se_age

overall_mean_age
ci_age




# Similar procedure for amount spent
spend_summary <- travel %>%
  group_by(spend_strata) %>%
  summarise(mean_spent = mean(amount_spent),
            sd_spent = sd(amount_spent),
            n = n())

overall_mean_spent <- sum(spend_summary$mean_spent * spend_summary$n) / sum(spend_summary$n)
se_spent <- sqrt(sum((spend_summary$n/N)^2 * (spend_summary$sd_spent^2 / spend_summary$n)))
ci_spent <- overall_mean_spent + c(-1,1) * qnorm(0.975) * se_spent

overall_mean_spent
ci_spent


anova_result <- aov(amount_spent ~ age_strata, data=travel)
summary(anova_result)

# If significant, post-hoc test
TukeyHSD(anova_result)

library(ggplot2)
# Age Distribution
ggplot(travel, aes(x=age_numeric)) +
  geom_histogram(binwidth=2, fill="skyblue") +
  labs(title="Age Distribution of Travelers", x="Age", y="Count")

# Amount Spent Distribution
ggplot(travel, aes(x=amount_spent)) +
  geom_histogram(binwidth=250, fill="orange") +
  labs(title="Amount Travelers Will Spend", x="Amount ($)", y="Count")

# Popular Destinations
dest_freq <- unlist(strsplit(travel$preferred_destinations, "; "))
barplot(table(dest_freq), col="lightgreen", las=2, main="Popular Travel Destinations")

#################################################################################################33
#Drug Analysis
########################################################################33

# Amount spent on drugs ($ per month)
set.seed(123)
drug$amount_spent_drugs <- round(runif(nrow(drug), 50, 2000), 0)

# How first introduced
drug$first_intro <- sample(c("Friends","Family","Online","Curiosity","Other"), nrow(drug), replace=TRUE)

# Do they want help? Yes/No
drug$want_help <- sample(c("Yes","No"), nrow(drug), replace=TRUE, prob=c(0.6,0.4))


# Age numeric already exists
summary(drug$age_numeric)

# Family background (using education of parents, e.g.)
# Let's assume drug$family_background exists, else create:
drug$family_background <- sample(c("Supportive","Neutral","Dysfunctional"), nrow(drug), replace=TRUE)
table(drug$family_background)


mean_spent_drugs <- mean(drug$amount_spent_drugs)
sd_spent_drugs <- sd(drug$amount_spent_drugs)
n_drug <- nrow(drug)
se_spent_drugs <- sd_spent_drugs / sqrt(n_drug)
ci_spent_drugs <- mean_spent_drugs + c(-1,1) * qnorm(0.975) * se_spent_drugs
mean_spent_drugs
ci_spent_drugs

prop_help <- mean(drug$want_help=="Yes")
se_help <- sqrt(prop_help*(1-prop_help)/n_drug)
ci_help <- prop_help + c(-1,1) * qnorm(0.975) * se_help
prop_help
ci_help

# Age Distribution
ggplot(drug, aes(x=age_numeric)) +
  geom_histogram(binwidth=2, fill="salmon") +
  labs(title="Age Distribution of Drug Users", x="Age", y="Count")

# Amount spent on drugs
ggplot(drug, aes(x=amount_spent_drugs)) +
  geom_histogram(binwidth=100, fill="purple") +
  labs(title="Monthly Spending on Drugs", x="Amount ($)", y="Count")

# First introduction
barplot(table(drug$first_intro), col="lightblue", main="How They Were First Introduced to Drugs")

# Want Help proportion
barplot(table(drug$want_help), col=c("red","green"), main="Proportion Wanting Help")







