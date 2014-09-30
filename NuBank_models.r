library(psych)
library(xtable)

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



dat1  <-  data.frame(dat1$scores) 
dat1$items  <-  rownames(data1) 
ggplot(dat1, aes(Comp.1, Comp.2, colour = items)) + geom_point() + 
  theme(legend.position="none") 

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