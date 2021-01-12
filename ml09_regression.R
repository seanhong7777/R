# lm(), rpart()를 사용한 red wine 품질 예측 

library(tidyverse)     # 데이터 전처리, 가공, 시각화
library(ModelMetrics)  # 모델 평가 지표
library(psych)         # pairs.panels() 
library(rpart)         # regression tree 
library(rpart.plot)    # regression tree vosualization 
library(class)
search()

# 1. Data preparation
red_wine <- read.csv(file = 'datasets/redwines.csv')
head(red_wine)
str(red_wine)
summary(red_wine)
# 관심(종속) 변수: quality(와인 품질/벌점)
# 대부분의 와인의 점수는 5~6점 

ggplot(data = red_wine)+
  geom_bar(aes(x = quality))

# 훈련/테스트 셋 분리(8:2)
N <- nrow(red_wine)   
train_size <- N * 0.8

X_train <- red_wine[1:train_size, ] 
X_test <- red_wine[(train_size + 1):N, ]

prop.table(table(X_train$quality))
prop.table(table(X_test$quality))

head(X_train)
str(X_train)

# 2. Linear Regression (lm) 모델을 학습 ---
# Linear Regression 모델 학습
lin_reg1 <- lm(formula = quality ~ ., data = X_train) 
lin_reg1
summary(lin_reg1)                                              #> Standard Error 0.6505 
## 여기 standard error는 train set의 RMSE와 비슷하게 나타남

# 훈련 셋의 RMSE, MAE 계산 
train_predicts1 <- predict(object = lin_reg1, newdata = X_train)
## lin_reg$fitted.values
rmse(actual = X_train$quality, predicted = train_predicts1)      #> 0.6474834
mae(actual = X_train$quality, predicted = train_predicts1)       #> 0.5012938

# 테스트 셋의 RMSE, MAE 계산 
test_predicts1 <- predict(object = lin_reg1, newdata = X_test)
rmse(actual = X_test$quality, predicted = test_predicts1)      #> 0.6397721
mae(actual = X_test$quality, predicted = test_predicts1)       #> 0.5049402

# 테스트 셋에서 residual(= 실제값 - 예측값) 들의 분포 
test_residuals <- X_test$quality - test_predicts1
summary(test_residuals)

# 3. Scaling + Liner Regression ---
# Quality를 제외한 모든 변수들을 scaling(정규화, 표준화) 후
# Linear Regression 모델을 학습시키고 위의 결과와 비교 

# 표준화(standardization) : 변수들의 평균을 0, 표준편차 1로 스케일링
std_scaler <- function(x) {
  return((x - mean(x)) / sd(x))
}

df <- red_wine[, 1:11] # Quality를 제외한 데이터프레임
head(df)

# df의 모든 변수들에 std_scaler를 적용
df_scaled <- data.frame(lapply(X = df, FUN = std_scaler))
summary(df_scaled)
sd(df_scaled$alcohol)

# 스케일링이 끝난 데이터프레임에 quality를 추가 
df_scaled$quality <- red_wine$quality
head(df_scaled)

# train set / test set split
X_train2 <- df_scaled[1:train_size, ]
X_test2 <- df_scaled[(train_size + 1):N, ]

# 훈련 셋 학습 
lin_reg2 <- lm(formula = quality ~ ., data = X_train2)
summary(lin_reg2)
#> Residual standard error: 0.6505
#> Multiple R-squared:  0.3443,	Adjusted R-squared:  0.3386
test_predicts2 <- predict(object = lin_reg2, newdata = X_train2)
rmse(X_test2$quality, test_predicts2)
mae(X_test2$quality, test_predicts2)

# 정규화
# min_max_normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# red_wine_norm <- data.frame(red_wine)


# 4. Regression Tree 모델을 학습(scaling되지 않은 변수들을 사용) ---
# 훈련 셋/테스트 셋의 RMSE 계산해서 Linear Regression의 결과들과 비교 
reg_tree <- rpart(formula = quality ~., data = X_train)
reg_tree
rpart.plot(reg_tree, cex = 0.9)
summary(reg_tree)

# 훈련 셋 예측
train_predicts3 <- predict(object = reg_tree, newdata = X_train)
# 훈련 셋 residuals 요약
summary(X_train$quality - train_predicts3)
# 훈련 셋 RMSE, MAE
rmse(X_train$quality, train_predicts3)   #> 0.6245433
mae(X_train$quality, train_predicts3)    #> 0.5066823

# 테스트 셋 예측
test_predicts3 <- predict(object = reg_tree, newdata = X_test)
# 테스트 셋 residuals 요약
summary(X_test$quality - test_predicts3)
# 테스트트 셋 RMSE, MAE
rmse(X_test$quality, test_predicts3)   #> 0.6578461
mae(X_test$quality, test_predicts3)    #> 0.5384235
