# Random Forest 회귀(Regression)
# Concrete 강도(strength) 예측 

library(tidyverse)
library(randomForest)
library(ModelMetrics) #mse(), rmse(), mae()
search()

# 1. Data import ---
concrete <- read.csv(file = 'datasets/concrete.csv')
str(concrete)

psych::pairs.panels(concrete)

# 2. train set/test set split = 8:2 ---
N <- nrow(concrete)
train_size <- round(N * 0.8)

X_train <- concrete[1:train_size, ]
X_test <- concrete[(train_size+1):N, ]

summary(X_train$strength)
summary(X_test$strength)

# 3. Random Forest 모델을 train set에서 학습 ---
forest_reg <- randomForest(formula = strength ~ ., data = X_train)
forest_reg
#> Mean of squared residuals: 32.31126

# 훈련된 모델을 훈련 셋에서 평가
train_pred <- predict(object = forest_reg, newdata = X_train)
mse(X_train$strength, train_pred)  #> 10.1186

# 훈련된 모델을 테스트 셋에서 평가
test_pred <- predict(object = forest_reg, newdata = X_test)
mse(X_test$strength, test_pred)    #> 31.92739
rmse(X_test$strength, test_pred)   #> 5.650433


# Random Forest 회귀를 사용한 의료비 지출 예측 
# 1. Data preparation
insurance <- read.csv(file = 'datasets/insurance.csv', stringsAsFactors = TRUE)
str(insurance)
summary(insurance)

# 2. Train/Test set split
N <- nrow(insurance)
train_size <- round(N * 0.8)
X_train <- insurance[1:train_size, ]
X_test <- insurance[(train_size + 1):N, ]

summary(X_train$expenses)
summary(X_test$expenses)

# 3. Random Forest 모델 학습
forest_reg2 <- randomForest(formula = expenses ~ ., data = X_train)
forest_reg2

# 학습된 모델의 훈련 셋에서의 평가
train_pred <- predict(object = forest_reg2, newdata = X_train)
summary(X_train$expenses - train_pred)
mse(X_train$expenses, train_pred)   #> 9696879
rmse(X_train$expenses, train_pred)  #> 3113.981

# 모델을 테스트 셋에서 평가
test_pred <- predict(object = forest_reg2, newdata = X_test)
summary(X_test$expenses - test_pred)
mse(X_test$expenses, test_pred)     #> 21731570
rmse(X_test$expenses, test_pred)    #> 4661.713
