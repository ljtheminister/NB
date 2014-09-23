library(ggplot2)
library(reshape2)
library(rgdal)


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
data$bank_account_duration <- factor(data$bank_account_duration)
data$salary_frequency <- factor(data$salary_frequency)
data$home_phone_type <- factor(data$home_phone_type)
data$other_phone_type <- factor(data$other_phone_type)
data$Gender_facebook <- factor(data$Gender_facebook)
data$Title <- factor(data$Title)

data$Gender_facebook_female <- ifelse(data$Gender_facebook=='female', 1, 0)
data$Credit_Line_approved_pct <- as.numeric(as.character(data$Credit_Line_approved_pct))
data$applicant_age <- as.numeric(as.character(data$applicant_age))

X_num <- data[, c(4, 19:24, 40, 42, 43)]
names(X_num) <- names(data)[c(4, 19:24, 40, 42, 43)]
X_cat <- data[, c(7:8, 13:18, 26)]
names(X_cat) <- names(data)[c(7:8, 13:18, 26)]

X_num_whitened <- scale(X_num, center=TRUE, scale=TRUE)

X <- cbind(X_num_whitened, X_cat)

X_geo <- cbind(X, data[, c(37, 38)])

y <- ifelse(data$y==-1, 0, data$y)
## Principle Component Analysis

PC <- princomp(X_num_whitened)
PC$loadings
plot(PC)
screeplot(PC, type='lines')

pc <- prcomp(X_num_whitened)
summary(pc)

data2 <- cbind(X, y)

## Factor Analysis

## Logistic Regression
logit_full <- glm(y ~ ., data=X, family='binomial')
summary(logit_full)

logit <- glm(y ~ raw_unit4_score + applicant_age + Gender_facebook_female, data=X, family='binomial')
summary(logit)

logit <- glm(y ~ raw_unit4_score + applicant_age + monthly_rent_amount, data=X, family='binomial')
summary(logit)


logit_back <- step(logit_full)


cor(data$raw_unit4_score, data$raw_lexisnexis_score)
cor(data$raw_unit4_score, data$raw_TU_score)
cor(data$raw_unit4_score, data$raw_FICO_money_score)

cor(data$raw_lexisnexis_score, data$raw_TU_score)
cor(data$raw_lexisnexis_score, data$raw_FICO_money_score)

cor(data$raw_TU_score, data$raw_FICO_money_score)


# Spatial Regression
counties<-readOGR("/BRA_adm/BRA_adm0.shp", layer="BRA0")

ggplot() +  geom_polygon(data=data, aes(x=Longitude, y=Latitude))
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y), color="red")
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y), color="blue")
