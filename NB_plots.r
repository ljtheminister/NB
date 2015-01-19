library(ggplot2)
library(reshape2)
library(rgdal)
library(maptools)

setwd('~/ML/NuBank')
data <- read.csv('data.csv')

## Data Pre-Processing
data$Facebook.profile.duration <- factor(data$Facebook.profile.duration)
data$residence_duration <- factor(data$residence_duration)
data$bank_account_duration <- factor(data$bank_account_duration, c('6 months or less', '7-12 months', '1-2 years', '3+ years'))
data$salary_frequency <- factor(data$salary_frequency, c('Weekly', 'Bi-weekly', 'Monthly', 'Semi-monthly'))
data$home_phone_type <- factor(data$home_phone_type)
data$other_phone_type <- factor(data$other_phone_type)
data$Gender_facebook <- factor(data$Gender_facebook)
data$Title <- factor(data$Title)
data$residence_rent_or_own <- factor(data$residence_rent_or_own)
data$residence_duration <- factor(data$residence_duration, c('6 months or less', '7-12 months', '1-2 years', '3+ years'))
data$Gender_facebook_female <- ifelse(data$Gender_facebook=='female', 1, 0)
data$Credit_Line_approved_pct <- as.numeric(as.character(data$Credit_Line_approved_pct))
data$applicant_age <- as.numeric(as.character(data$applicant_age))



## EXPLORATORY DATA ANALYSIS @ Individual Variable Level

# Overlayed Histograms in ggplot2
setwd('~/ML/NuBank/Plots')

# AGE
ggplot(data, aes(x=applicant_age, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Applicant Age') +
  xlab('Applicant Age (years)') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='applicant_age_repayment_density.pdf', width=297, height=210, units="mm")

ggplot(data, aes(x=age2, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Applicant Age Squared') +
  xlab('Applicant Age Squared (years)') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='applicant_age2_repayment_density.pdf', width=297, height=210, units="mm")


# GENDER
ggplot(data, aes(x=Gender_facebook, fill=factor(y))) + geom_bar(position="fill") +   
  ggtitle('Proportion Plot of Loan Repayment Status vs. Gender on Facebook') +
  xlab('Gender') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='gender_repayment_density.pdf', width=297, height=210, units="mm")



# Monthly Income vs. Salary Frequency
ggplot(data, aes(x=monthly_income_amount, fill=salary_frequency)) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Monthly Income by Salary Frequency')
  xlab('Monthly Income')
  scale_fill_discrete('Salary Frequency')

ggsave(file='monthly_income_salary_frequency_density.pdf', width=297, height=210, units="mm")


# Monthly Income
ggplot(data, aes(x=monthly_income_amount, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Reported Monthly Income') +
  xlab('Reported Monthly Income') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='monthly_income_repayment_density.pdf', width=297, height=210, units="mm")

# Monthly Rent
ggplot(data, aes(x=monthly_rent_amount, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Reported Monthly Rent') +
  xlab('Reported Monthly Rent') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
        
ggsave(file='monthly_rent_repayment_density.pdf', width=297, height=210, units="mm")
                      
## CREDIT SCORES
# Unit 4 score
ggplot(data, aes(x=raw_unit4_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Unit 4 Score') +
  xlab('Unit 4 Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='unit4_repayment_density.pdf', width=297, height=210, units="mm")

# Serasa score
ggplot(data, aes(x=raw_serasa_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Serasa Experian Score') +
  xlab('Serasa Experian Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
                      
ggsave(file='serasa_repayment_density.pdf', width=297, height=210, units="mm")

#LexisNexis score
ggplot(data, aes(x=raw_lexisnexis_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Lexis Nexis Score') +
  xlab('Lexis Nexis Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='lexisnexis_repayment_density.pdf', width=297, height=210, units="mm")
                        
# TransUnion score                                             
ggplot(data, aes(x=raw_TU_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. TransUnion Score') +
  xlab('TransUnion Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='tu_repayment_density.pdf', width=297, height=210, units="mm")

# FICO score                      
ggplot(data, aes(x=raw_FICO_money_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. FICO Score') +
  xlab('FICO Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))
                      
ggsave(file='fico_repayment_density.pdf', width=297, height=210, units="mm")
                      
                      
## CREDIT LINE APPLICATION                      
ggplot(data, aes(x=Credit_Line_approved_pct, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Credit Line Approved Percent (%)') +
  xlab('Credit Line Approved / Credit Line Requested (%)') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
                      
ggsave(file='CL_approved_pct_repayment_density.pdf', width=297, height=210, units="mm")


ggplot(data, aes(x=Credit_Line_requested, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Requested Credit Line ') +
  xlab('Requested Credit Line Amount') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 

ggsave(file='CL_requested_repayment_density.pdf', width=297, height=210, units="mm")

                      
ggplot(data, aes(x=Credit_Line_approved, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Approved Credit Line') +
  xlab('Approved Credit Line Amount') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
                   
ggsave(file='CL_approved_repayment_density.pdf', width=297, height=210, units="mm")



# Salary Frequency
ggplot(data, aes(x=salary_frequency, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Salary Frequency of Applicant') +
  xlab('Salary Frequency') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='salary_frequency_repayment_density.pdf', width=297, height=210, units="mm")

# Bank Account Duration
ggplot(data, aes(x=bank_account_duration, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Duration of Bank Account') +
  xlab('Bank Account Duration') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='bank_account_duration_repayment_density.pdf', width=297, height=210, units="mm")


ggplot(data, aes(x=home_phone_type, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Salary Frequency of Applicant') +
  xlab('Salary Frequency') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='salary_frequency_repayment_density.pdf', width=297, height=210, units="mm")

ggplot(data, aes(x=other_phone_type, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Salary Frequency of Applicant') +
  xlab('Salary Frequency') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='salary_frequency_repayment_density.pdf', width=297, height=210, units="mm")


ggplot(data, aes(x=residence_rent_or_own, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Salary Frequency of Applicant') +
  xlab('Salary Frequency') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='salary_frequency_repayment_density.pdf', width=297, height=210, units="mm")

ggplot(data, aes(x=residence_duration, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Residence Duration of Applicant') +
  xlab('Residence Duration') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='residence_duration_repayment_density.pdf', width=297, height=210, units="mm")
