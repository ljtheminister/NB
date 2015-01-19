library(ggplot2)
library(reshape2)
library(rgdal)
library(maptools)
library(e1071)

setwd('~/ML/NuBank')
data <- read.csv('data.csv')

plot(data[data$y==1, 'Facebook.profile.duration'])
plot(data[data$y==-1, 'Facebook.profile.duration'])


hist(data[data$y==1, 'monthly_income_amount'])
hist(data[data$y==-1, 'monthly_income_amount'])

hist(data[data$y==1, 'Credit_Line_approved'])
hist(data[data$y==-1, 'Credit_Line_approved'])

#data$Credit_Line_approved_pct <- data$Credit_Line_approved_pct
hist(data[data$y==1, 'Credit_Line_approved_pct'])
plot(data[data$y==-1, 'Credit_Line_approved_pct'])
plot(data[data$y==1, 'Credit_Line_approved_pct'])


hist(data[data$y==1, 'raw_unit4_score'])
hist(data[data$y==-1, 'raw_unit4_score'])

hist(data[data$y==1, 'raw_serasa_score'])
hist(data[data$y==-1, 'raw_serasa_score'])

hist(data[data$y==1, 'raw_TU_score'])
hist(data[data$y==-1, 'raw_TU_score'])

hist(data[data$y==1, 'raw_lexisnexis_score'])
hist(data[data$y==-1, 'raw_lexisnexis_score'])

plot(data[data$y==1, 'bank_account_duration'])
plot(data[data$y==-1, 'bank_account_duration'])

plot(data[data$y==1, 'residence_duration'])
plot(data[data$y==-1, 'residence_duration'])

plot(data[data$y==1, 'salary_frequency'])
plot(data[data$y==-1, 'salary_frequency'])

# histogram of income vs. salary frequency

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



cor(data$raw_unit4_score, data$raw_lexisnexis_score)
cor(data$raw_unit4_score, data$raw_TU_score)
cor(data$raw_unit4_score, data$raw_FICO_money_score)

cor(data$raw_lexisnexis_score, data$raw_TU_score)
cor(data$raw_lexisnexis_score, data$raw_FICO_money_score)

cor(data$raw_TU_score, data$raw_FICO_money_score)


# Spatial Regression
bra0 <-readOGR("BRA_adm/BRA_adm0.shp", layer='BRA_adm0')
bra1 <-readOGR("BRA_adm/BRA_adm1.shp", layer='BRA_adm1')
bra2 <-readOGR("BRA_adm/BRA_adm2.shp", layer='BRA_adm2')

shape <- readShapeSpatial("BRA_adm/BRA_adm0.shp")

shape@data$id = rownames(shape@data)
shape@data$id = rownames(shape@data)
shape.points = fortify(shape, region="id")
shape.df = join(shape.points, shape@data, by="id")

shape.fort <- fortify(shape, region='id') 
shape.fort<-shape.fort[order(shape.fort$order), ] 
ggplot(data=shape.fort, aes(long, lat, group=group)) + 
  geom_polygon(colour='black',
               fill='white') +
  theme_bw()

b_mod <- fortify(b)? 
? fortify

good_loans = subset(data, y==1)
bad_loans = subset(data, y==-1)

plot(bra0)
points(x=good_loans$Longitude, y=good_loans$Latitude, col='green', cex=0.25)
points(x=bad_loans$Longitude, y=bad_loans$Latitude, col='red', cex=0.25)

brazil <- readOGR('BRA_adm/BRA_adm0.shp', layer='BRA_adm0')
ggplot() + geom_polygon(data=brazil, aes(x=long, y=lat, group=group))

ggplot() +  geom_polygon(data=bra0, aes(x=Longitude, y=Latitude))
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y==1), color="red")
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y==-1), color="blue")

data$good <- ifelse(data$y==1, 'Good', 'Bad')
data$good <- factor(data$good)
## EXPLORATORY DATA ANALYSIS @ Individual Variable Level

# Overlayed Histograms in ggplot2

ggplot(data, aes(x=monthly_income_amount, fill=salary_frequency)) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Monthly Income by Salary Frequency')
  xlab('Monthly Income')
  scale_fill_discrete('Salary Frequency')

ggsave(file='monthly_income_salary_frequency_density.pdf', width=297, height=210, units="mm")



ggplot(data, aes(x=monthly_income_amount, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Reported Monthly Income') +
  xlab('Reported Monthly Income') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='monthly_income_repayment_density.pdf', width=297, height=210, units="mm")

ggplot(data, aes(x=monthly_rent_amount, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Reported Monthly Rent') +
  xlab('Reported Monthly Rent') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
        
ggsave(file='monthly_rent_repayment_density.pdf', width=297, height=210, units="mm")
                      

ggplot(data, aes(x=raw_unit4_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Unit 4 Score') +
  xlab('Unit 4 Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='unit4_repayment_density.pdf', width=297, height=210, units="mm")

ggplot(data, aes(x=raw_serasa_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Serasa Score') +
  xlab('Serasa Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
                      
ggsave(file='serasa_repayment_density.pdf', width=297, height=210, units="mm")

  
ggplot(data, aes(x=raw_lexisnexis_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Lexis Nexis Score') +
  xlab('Lexis Nexis Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='lexisnexis_repayment_density.pdf', width=297, height=210, units="mm")
                        
                        

                      
ggplot(data, aes(x=raw_TU_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. TU Score') +
  xlab('TU Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='tu_repayment_density.pdf', width=297, height=210, units="mm")

                      
ggplot(data, aes(x=raw_FICO_money_score, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. FICO Score') +
  xlab('FICO Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))
                      
ggsave(file='fico_repayment_density.pdf', width=297, height=210, units="mm")
                      
                      
                      
ggplot(data, aes(x=Credit_Line_approved_pct, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Credit Line Approved Percent (%)') +
  xlab('Credit Line Approved / Credit Line Requested (%)') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
                      
ggsave(file='CL_approved_pct_repayment_density.pdf', width=297, height=210, units="mm")


ggplot(data, aes(x=Credit_Line_requested, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Credit Line Requested ') +
  xlab('Credit Line Requested') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 

ggsave(file='CL_requested_repayment_density.pdf', width=297, height=210, units="mm")

                      
ggplot(data, aes(x=Credit_Line_approved, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Unit 4 Score') +
  xlab('Unit 4 Score') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months')) 
                   
ggsave(file='CL_approved_repayment_density.pdf', width=297, height=210, units="mm")



ggplot(data, aes(x=applicant_age, fill=factor(y))) + geom_density(alpha=0.5) +
  ggtitle('Density Plot of Loan Repayment Status vs. Applicant Age') +
  xlab('Applicant Age (years)') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))
                      
ggsave(file='applicant_age_repayment_density.pdf', width=297, height=210, units="mm")
                      

ggplot(data, aes(x=salary_frequency, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Salary Frequency of Applicant') +
  xlab('Salary Frequency') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='salary_frequency_repayment_density.pdf', width=297, height=210, units="mm")

ggplot(data, aes(x=bank_account_duration, fill=factor(y))) + geom_bar(position="fill") + 
  ggtitle('Proportion Plot of Loan Repayment Status vs. Duration of Bank Account') +
  xlab('Bank Account Duration') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='bank_account_duration_repayment_density.pdf', width=297, height=210, units="mm")


ggplot(data, aes(x=Gender_facebook, fill=factor(y))) + geom_bar(position="fill") +   
  ggtitle('Proportion Plot of Loan Repayment Status vs. Gender on Facebook') +
  xlab('Gender') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='gender_repayment_density.pdf', width=297, height=210, units="mm")

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
  xlab('Salary Frequency') +
  scale_fill_discrete('Loan Repayment Status', labels=c('Outstanding', 'Paid back in 12 months'))

ggsave(file='residence_duration_repayment_density.pdf', width=297, height=210, units="mm")


plot(factor(data$residence_duration), factor(data$y))
table(data$residence_duration, data$y)

plot(factor(data$Title), factor(data$y))
sort(table(data$City), decreasing=TRUE)
RJ <- subset(data, City=='Rio de Janeiro')
ggplot(RJ, aes(x=raw_lexisnexis_score, fill=factor(y))) + geom_density(alpha=0.5)
table(RJ$y)

cities <- levels(data$City)


SP <- subset(data, City==cities[183])
ggplot(SP, aes(x=raw_lexisnexis_score, fill=factor(y))) + geom_density(alpha=0.5)
table(SP$y)

Sal <- subset(data, City=='Salvador')
RJSPSL <- rbind(RJ, SP, Sal)

RJSP <- rbind(RJ, SP)
RJSP_good <- subset(RJSP, y==1)
RJSP_bad <- subset(RJSP, y==-1)

ggplot(RJSP, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSP_good, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSP_bad, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)

RJSPSL_good <- subset(RJSPSL, y==1)
RJSPSL_bad <- subset(RJSPSL, y==-1)

ggplot(RJSPSL, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSPSL_good, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)
ggplot(RJSPSL_bad, aes(x=raw_lexisnexis_score, fill=factor(City))) + geom_density(alpha=0.5)

plot(bra0)
points(x=RJSPSL_good$Longitude, y=RJSPSL_good$Latitude, col='green', cex=0.25)
points(x=RJSPSL_bad$Longitude, y=RJSPSL_bad$Latitude, col='red', cex=0.25)

dim(RJSPSL_good)
dim(RJSPSL_bad)
