# Regression Tree(회귀 나무): Decision Tree를 사용한 수치 예측

library(tidyverse)
library(ModelMetrics)
library(psych)
library(rpart)
library(rpart.plot)
search()

# 1. Data set import ---
insurance <- read.csv(file = 'datasets/insurance.csv', stringsAsFactors = TRUE)
head(insurance)

# 훈련 셋/테스트 셋 분리
N <- nrow(insurance)   # 데이터프레임의 row 개수
train_size <- round(N * 0.8)  
X_train <- insurance[1:train_size, ]
X_test <- insurance[(train_size + 1):N, ]

# 2. Regression Tree 모델을 학습 --- 
# RPART: Recursive Partitioning and Regression Tree
reg_tree <- rpart(formula = expenses ~ ., data = X_train)
reg_tree
rpart.plot(reg_tree)

mean(X_train$expenses)
X_train %>% 
  filter(smoker == 'no' & age < 42.5) %>% 
  summarize(mean(expenses))

summary(reg_tree)

# 훈련셋의 RMSE:
train_predicts <- predict(object = reg_tree, newdata = X_train)
rmse(actual = X_train$expenses, predicted = train_predicts)  #> 5012.461

# regression tree를 사용해서 test set의 expenses 예측값 계산 
test_predict <- predict(object = reg_tree, newdata = X_test)

# regression tree 모델의 RMSE를 추정 
rmse(actual = X_test$expenses, predicted = test_predict)   #> 5104.558

# reg_tree 모델의 summary에서 변수들의 중요도(variable importance)를 알 수 있음
# -> smoker(70), bmi(17)
# -> insurance 데이터프레임에 overweight_smoker
# -> bmi >= 30 & smoker == 'yes' 이면 1, 그렇지 않으면 0 
# -> train/test set split
# -> Regression Tree를 생성, 테스트 셋의 RMSE를 계산 

insurance_mod <- insurance %>% 
  mutate(overweight_smoker = ifelse(bmi >30 & smoker == 'yes', 1, 0))
head(insurance_mod)
tail(insurance_mod)

insurance_mod %>% filter(overweight_smoker == 1) %>% count()

X_train2 <- insurance_mod[1:train_size, ]
X_test2 <- insurance_mod[(train_size + 1):N, ]

reg_tree2 <- rpart(formula = expenses ~., data = X_train2)
reg_tree2
rpart.plot(reg_tree2)

summary(reg_tree2)

# train set의 RMSE 
train_predicts2 <- predict(object = reg_tree2, newdata = X_train2)
rmse(actual = X_train$expenses, predicted = train_predicts2)  #> 5012.776

# test set의 RMSE
test_predictc2 <- predict(object = reg_tree2, newdata = X_test2)
rmse(actual = X_test$expenses, predicted = test_predictc2)  #> 5131.905

# insurance 데이터 프레임에 파생 변수를 추가, 테스트 
# age_square = age^2 
# overweight = 1 if bmi >= 30, else 0 

