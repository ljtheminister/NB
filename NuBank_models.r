library(psych)
library(xtable)
library(caret)
library(pROC)
library(MASS)
library(e1071)

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
data$age2 <- data$applicant_age^2
data$income2 <- data$monthly_income_amount^2
data$rent2 <- data$monthly_rent_amount^2


X_num <- data[, c(4, 10, 19:24, 40, 42:46)]
names(X_num) <- names(data)[c(4, 10, 19:24, 40, 42:46)]
X_cat <- data[, c(8, 13:18, 26)]
names(X_cat) <- names(data)[c(8, 13:18, 26)]

X_num_whitened <- scale(X_num, center=TRUE, scale=TRUE)

X <- cbind(X_num_whitened, X_cat)

X_geo <- cbind(X, data[, c(37, 38)])

y <- ifelse(data$y==-1, 0, data$y)
data2<- cbind(X,y)
data2 <- na.omit(data2)

X <- data2[, 1:22]
y <- data2$y
## Principle Component Analysis

PC <- princomp(X_num_whitened)
PC$loadings
plot(PC)
screeplot(PC, type='lines')
vars <- apply(PC$x, 2, var)


ggscreeplot <- function(pcobj, type = c('pev', 'cev')) 
{
  type <- match.arg(type)
  d <- pcobj$sdev^2
  yvar <- switch(type, 
                 pev = d / sum(d), 
                 cev = cumsum(d) / sum(d))
  
  yvar.lab <- switch(type,
                     pev = 'proportion of explained variance',
                     cev = 'cumulative proportion of explained variance')
  
  df <- data.frame(PC = 1:length(d), yvar = yvar)
  
  ggplot(data = df, aes(x = PC, y = yvar)) + 
    xlab('principal component number') + ylab(yvar.lab) +
    geom_point() + geom_path()
}

setwd('~/ML/NuBank/Plots')

ggscreeplot(PC) +
  ggtitle('Screeplot of Principal Components')

ggsave(file='screeplot.pdf', width=297, height=210, units="mm")

## Factor Analysis


# 80-20 test training split
N = nrow(X)
test_split_prop <- .20
test_rows <- sample(1:N, test_split_prop*N, replace=FALSE)

X_train <- X[-test_rows,]
X_test <- X[test_rows,]
y_train <- y[-test_rows]
y_test <- y[test_rows]

### BUILD MODELS

## Logistic Regression

full_model <- glm(y_train ~ ., data=X_train, family='binomial')
full_pred <- predict(full_model, newdata=X_test)
summary(full_model)
roc_full <- roc(y_test, full_pred)
plot(roc_full)

null_model <- glm(y_train ~ 1, data=X_train, family='binomial')
null_pred <- predict(null_model, newdata=X_test)
summary(null_model)
roc_null <- roc(y_test, null_pred)
plot(roc_null)


backward_model <- stepAIC(full_model, scope=list(upper=full_model, lower=null_model), direction="backward")
back_pred <- predict(backward_model, newdata=X_test)
summary(backward_model)
roc_back <- roc(y_test, back_pred)
plot(roc_back)


forward_model <- stepAIC(null_model, scope=list(upper=full_model, lower=null_model), direction="forward")
fwd_pred <- predict(forward_model, newdata=X_test)
summary(forward_model)
roc_fwd <- roc(y_test, fwd_pred)
plot(roc_fwd)

both_back <- stepAIC(backward_model, scope=list(upper=full_model, lower=null_model), direction="both")
both_forw <- stepAIC(forward_model, scope=list(upper=full_model, lower=null_model), direction="both")




# 10-Fold Cross-Validation

folds <- createFolds(y, 10)

k_fold_cv_error

for (i in 1:10) {
  test_rows <- folds[i]
  X_train <- X[-test_rows,]
  X_test <- X[test_rows,]
  y_train <- y[-test_rows]
  y_test <- y[test_rows]

  
  
}







p_full <- ifelse(p_full>0.5, 1, 0)
sum(p_full == y_test)/length(y_test)


p1_pred <- ifelse(p1>0.5, 1, 0)
sum(p1_pred == y_test)/length(y_test)


p2_pred <- ifelse(p2>0.5, 1, 0)

sum(p2_pred == y_test)/length(y_test)

logit_back <- step(logit_full)

logitback <- glm(y_train ~ raw_lexisnexis_score + Credit_Line_approved_pct + applicant_age + salary_frequency, scope = y_train ~ ., data=X_train, family = 'binomial')
stepAIC(logitback, direction="forward")

summary(logitback)
p_back <- predict(logitback, newdata=X_test)
p_back_pred <- ifelse(p_back>0.5, 1, 0)

sum(p_back_pred == y_test)/length(y_test)

logit_step_both <- step(logit_full, direction="both")
# GOT TO ADD SCOPE TO both directions of step-wise logistic regression


svm1 <- svm(y_train ~ raw_lexisnexis_score + applicant_age + Gender_facebook_female, data=X_train)
svm_p1 <- predict(svm1, newdata=X_test)
roc_svm1 <- roc(y_test, svm_p1)
plot(roc_svm1)

svm_pred1 <- ifelse(svm_p1>0.5, 1, 0)

sum(svm_pred1 == y_test)/length(y_test)

svm2 <- svm(y_train ~ raw_lexisnexis_score + applicant_age + monthly_rent_amount, data=X_train)
svm_p2 <- predict(svm1, newdata=X_test)
roc_svm2 <- roc(y_test, svm_p2)
plot(roc_svm2)



svm_pred2 <- ifelse(svm_p2>0.5, 1, 0)
sum(svm_pred2 == y_test)/length(y_test)

svm_full <- svm(y_train ~ ., data=X_train)
svm_full <- predict(svm_full, newdata=X_test)
svm_pred_full <- ifelse(svm_full>0.5, 1, 0)

svm_pred_full == y_test
sum(svm_pred_full == y_test)/length(y_test)