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
X_cat <- data[, c(8, 13:18, 26)]
names(X_cat) <- names(data)[c(8, 13:18, 26)]

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
N = nrow(X)
test_rows <- sample(1:N, 64, replace=FALSE)
X_train <- X[-test_rows,]
X_test <- X[test_rows,]
y_train <- y[-test_rows]
y_test <- y[test_rows]


logit_full <- glm(y_train ~ ., data=X_train, family='binomial')
summary(logit_full)

p_full <- predict(logit_full, newdata=X_test)
p_full <- ifelse(p_full>0.5, 1, 0)
sum(p_full == y_test)/length(y_test)

logit1 <- glm(y_train ~ raw_unit4_score + applicant_age + Gender_facebook_female, data=X_train, family='binomial')
summary(logit1)

p1 <- predict(logit1, newdata=X_test)
p1_pred <- ifelse(p1>0.5, 1, 0)
sum(p1_pred == y_test)/length(y_test)

logit2 <- glm(y_train ~ raw_unit4_score + applicant_age + monthly_rent_amount, data=X_train, family='binomial')
summary(logit2)

p2 <- predict(logit2, newdata=X_test)
p2_pred <- ifelse(p2>0.5, 1, 0)

sum(p2_pred == y_test)/length(y_test)

logit_back <- step(logit_full)

logitback <- glm(y_train ~ raw_lexisnexis_score + Credit_Line_approved_pct + applicant_age + salary_frequency, data=X_train, family = 'binomial')

summary(logitback)
p_back <- predict(logitback, newdata=X_test)
p_back_pred <- ifelse(p_back>0.5, 1, 0)

sum(p_back_pred == y_test)/length(y_test)

logit_step_both <- step(logit_full, direction="both")
# GOT TO ADD SCOPE TO both directions of step-wise logistic regression


svm1 <- svm(y_train ~ raw_unit4_score + applicant_age + Gender_facebook_female, data=X_train)
svm_p1 <- predict(svm1, newdata=X_test)
svm_pred1 <- ifelse(svm_p1>0.5, 1, 0)

sum(svm_pred1 == y_test)/length(y_test)

svm2 <- svm(y_train ~ raw_unit4_score + applicant_age + monthly_rent_amount, data=X_train)
svm_p2 <- predict(svm1, newdata=X_test)
svm_pred2 <- ifelse(svm_p2>0.5, 1, 0)
sum(svm_pred2 == y_test)/length(y_test)

svm_full <- svm(y_train ~ ., data=X_train)
svm_full <- predict(svm_full, newdata=X_test)
svm_pred_full <- ifelse(svm_full>0.5, 1, 0)

svm_pred_full == y_test
sum(svm_pred_full == y_test)/length(y_test)

cor(data$raw_unit4_score, data$raw_lexisnexis_score)
cor(data$raw_unit4_score, data$raw_TU_score)
cor(data$raw_unit4_score, data$raw_FICO_money_score)

cor(data$raw_lexisnexis_score, data$raw_TU_score)
cor(data$raw_lexisnexis_score, data$raw_FICO_money_score)

cor(data$raw_TU_score, data$raw_FICO_money_score)


# Spatial Regression
bra0 <-readShapePoly("BRA_adm/BRA_adm0.shp", layer="BRA0")

ggplot() +  geom_polygon(data=data, aes(x=Longitude, y=Latitude))
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y), color="red")
ggplot() +  geom_point(data=data, aes(x=Longitude, y=Latitude, group=y), color="blue")

aall.