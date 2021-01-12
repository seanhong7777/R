# 선형 회귀(Linear Regression) - 의료비 예측 

# install.packages('psych')
# install.packages('rpart')
# install.packages('rpart.plot')
library(tidyverse)               # 데이터 전처리, 시각화
library(ModelMetrics)            # 모델 성능 지표 계산 
library(psych)                   # 향상된 scatter plot matrix(산포도 행렬)
library(rpart)                   # Regression Tree (Decision tree regression)
library(rpart.plot)              # REgression Tree 시각화 
search()

# 1. 데이터 준비 ---
insurance <- read.csv(file = 'datasets/insurance.csv', stringsAsFactors = TRUE)
str(insurance)
# bmi(bady mass index) = 몸무게/키^2 (kg/m^2)
# expenses: 의료비 지출 - 관심(종속) 변수
# expenses ~ age + sex + bmi +children + smoker + region 
# expenses = b0 + b1*age + b2*sex + b3*bmi + b4*children + b5*smoker + b6*region 

# 선형 회귀를 할 수 없는 문제점 
# sex, smoker, region 변수들은 문자열 타입이어서 산술연산을 할 수 없음 
# 문자열 타입을 factor 타입으로 변환해야 함 
# lm() 함수가 factor 타입 변수들을 숫자(0, 1, 2, ...)로 변환해서 회귀식을 찾아줌

summary(insurance)  #> 기술 통계량(descriptive statistics)

# age 분포 시각화 - histogram, boxplot
ggplot(data = insurance, aes(y = age)) + 
  geom_boxplot()
ggplot(data = insurance, aes(x = age)) + 
  geom_histogram(bins = 10, color = 'black', fill = 'lightgray')

# bim 분포 시각화 - histogram, boxplot
ggplot(data = insurance, aes(y = bmi)) + geom_boxplot()
ggplot(data = insurance, aes(x = bmi)) + 
  geom_histogram(bins = 10, color = 'black', fill = 'lightgray')

# expenses(의료비 지출) 분포 시각화 
ggplot(data = insurance, aes(y = expenses)) + 
  geom_boxplot()
ggplot(data = insurance, aes(x = expenses)) + 
  geom_histogram(bins = 15, color = 'black', fill = 'lightgray')

# expenses ~ age scatter matrix 
ggplot(data = insurance, aes(x = age, y = expenses)) + 
  geom_point()

# expenses ~ bmi scatter matrix
ggplot(data = insurance, aes(x = bmi, y = expenses)) + 
  geom_point()

# graphics::pairs() 함수: 산포도 행렬(scatter plot matrix)
pairs(insurance[c('age', 'bmi', 'children', 'expenses')])
pairs(insurance)

# psych::pairs.panels() 함수: 향상된 scatter plot matrix
#  대각선: 각 변수의 분포도(히스토그램, 막대그래프)
#  대각선 아래쪽: 두 변수 간의 scatter plot
#  대각선 위쪽: 두 변수 간의 상관 관계(correlation coefficient)
# 두 변수 간의 scatter plot
pairs.panels(insurance[c('age', 'bmi', 'children', 'expenses')])

# 상관계수 corr(x, y) = cov(x, y) / (sd(x)*sd(y))
#  -1 <= correlation coefficient <= 1
#  +1에 가까울수록 양의 상관관계, -1에 가까울수록 음의 상관관계
#  0에 가까울수록 상관관계가 없다고 말함 
#  (주의) 상관 관계와 인과 관계는 다르다 !!
# expenses, age의 상관 계수
cov(insurance$expenses, insurance$age) / (sd(insurance$expenses)*sd(insurance$age))
# expenses, bmi의 상관 계수
cov(insurance$expenses, insurance$bmi) / (sd(insurance$expenses)*sd(insurance$bmi))

cor(insurance$expenses, insurance$age)
cor(insurance$expenses, insurance$bmi)

pairs.panels(insurance)

# 3. 모델 훈련 ---
# 다향 선형 회귀 모델 생성, 훈련

# 데이터를 train set/test set 분리
train_size <- round(1338 * 0.8)

train_set <- insurance[1:train_size, ]
test_set <- insurance[(train_size+1):1338, ]

summary(train_set$expenses)
summary(test_set$expenses)

# lm은 label을 따로 분리하지 않는다
# 선형 회귀 모델을 train set으로 학습시킴(fitting to Linear model)
# formula = expenses ~ . 
# expenses ~ age + sex + bmi + children + smoker + region 
lin_reg1 <- lm(formula = expenses ~ ., data = train_set)
lin_reg1
# expenses = b0 + b1*age + b2*sexmale + b3*bmi + b4*children 
#           + b5*sm_yes + b6*r_nw + b7*r_se + b8*r_sw
# 카테고리 타입의 변수들은 one-hot encoding 되어서 선형회귀식에 포함됨 

# 선형회귀식의 계수들(절편과 각 변수의 기울기들)
lin_reg1$coefficients

# 모델이 찾은 선형회귀식을 사용해서 계산한 train set의 각 샘플들의 의료비 예측값 
lin_reg1$fitted.values[1:5]

# Train set에서 의료비 실제값
train_set$expenses

# residual: 오차 = 실제값 - 예측값 
train_set$expenses[1:5] - lin_reg1$fitted.values[1:5]
lin_reg1$residuals[1:5]

# residual 들의 요약 기술 통계량 요약 
summary(lin_reg1$residuals)
summary(train_set$expenses)

# train set 에서의 평가 지표: MSE, RMSE, MAE 
mse(actual = train_set$expenses, predicted = lin_reg1$fitted.values)   #> 36370529
rmse(actual = train_set$expenses, predicted = lin_reg1$fitted.values)  #> 6030.798
sqrt(mean(lin_reg1$residuals ^ 2))

mae(actual = train_set$expenses, predicted = lin_reg1$fitted.values)   #> 4151.354
mean(abs(lin_reg1$residuals))

# lm 모델에서는 mse와 rmse를 간단히 계산할 수 있음
mse(modelObject = lin_reg1)
rmse(modelObject = lin_reg1)
# mae()는 모델 객체로 계산할 수 없음!

# lm 모델 객체의 summary 
summary(lin_reg1)
#> call: 모델 함수. 선형회귀식. 데이터프레임
#> resioduals: 모델에서 계산된 residual(=실제값 - 예측값)들의 기술 통계량
#> coefficients: 모델이 찾은 선형회귀식의 계수들과 유의수준(significant levels)
#> residual standard error: 오차(residuals) 제곱 평균의 제곱근(RMSE)
#> Multiple R^2, Adjusted R^2: 모델이 훈련 셋을 얼마나 잘 설명하는 지를 측정한 값 

# 해석
#   Probability : 유의수준 (낮을 수록 필연적으로 일어날 확률이 높음 / 높을 수록 우연히) 
#   * 별세개 0.001 accept 수준 

# Residual standard error는 rmse와 동일하다고 봄

# R-squared: train set 중의 75% 정도는 설명을 잘한다고 봄 
# ("전체 데이터 중 75%를 적은 오차로 설명 가능하다". "
# 나머지 25%는 RMSE보다 더 큰 오차를 발생시킨다")

# RMSE: residual -> residual ^2 -> 평균 -> 제곱근
# residual standard error와 RMSE가 약간 차이가 나는 이유는
# 평균을 계산할 때 샘플의 개수로 나누는지 또는 자유도로 나누는 지의 차이 때문

sqrt(sum(lin_reg1$residuals ^2) / 1070)
sqrt(sum(lin_reg1$residuals ^2) / 1061)

rmse(lin_reg1) 

lin_reg1$call

# 4. 모델 평가 --- 
# test set 의 예측값, 평가 지표 계산 
test_predicts <- predict(object = lin_reg1,    # object = 훈련된 모델 
                         newdata = test_set)   # newdata = test set 

# test set 에서의 RMSE
rmse(actual = test_set$expenses, predicted = test_predicts)   #> 6096.883
# 일반적으로 train set의 RMSE보다 test set의 RMSE 커지는 경우가 일반적임 
#  의료비 예측 문제에서는, 훈련 셋과 테스트 셋의 RMSE 차이가 크지 않음
#  -> 과적합(overfitting)이 크지 않음 

# 5. 모델 변경, 평가 --- 
# 1)  expenses ~ age + sex + bmi + smoker 
lin_reg2 <- lm(formula = expenses ~ age + sex + bmi + smoker, data = train_set)
lin_reg2$coefficients
summary(lin_reg2)  #> Residual standard error: 6088
#> lin_reg1에 비해서 residual std. err.가 더 커졌음 (>32)

test_predicts2 <- predict(object = lin_reg2, newdata = test_set)
rmse(actual = test_set$expenses, predicted = test_predicts2)              #> 6126.623
#> Multiple R-squared:  0.745,	Adjusted R-squared:  0.7441 
#> Lin_reg1에 비해서 rmse 증가 
#> R^2 약간 작아짐

# 선형(linear) vs 비선형(non-linear) 
# 2) expenses ~ age^2 + sex + bmi + smoker 
# 2차 이상의 고차항들을 추가할 때는 파생변수를 추가해야 함
insurance_mod <- insurance %>% 
  mutate(age_square = age * age)
head(insurance_mod)

X_train <- insurance_mod[1:train_size, ]
X_test <- insurance_mod[(train_size + 1):1338, ]

lin_reg3 <- lm(formula = expenses ~ age_square + sex + bmi + smoker, 
               data = X_train)
lin_reg3
summary(lin_reg3)  #> Residual standard error: 6075
#> Multiple R-squared:  0.7461,	Adjusted R-squared:  0.7452

test_predicts3 <- predict(object = lin_reg3, newdata = X_test)
rmse(actual = X_test$expenses, predicted = test_predicts3)    #> 6103.207

# 2-2) age 를 추가로 
lin_reg4 <- lm(formula = expenses ~ age + age_square + sex + bmi + smoker, 
               data = X_train)
lin_reg4
summary(lin_reg4)  #> Residual standard error: 6076
# Multiple R-squared:  0.7463,	Adjusted R-squared:  0.7451 

test_predicts4 <- predict(object = lin_reg4, newdata = X_test)
rmse(actual = X_test$expenses, predicted = test_predicts4)    #>6104.617

## 선형회귀라고 해서 변수들의 1차항만 고려하는 것이 아니다.
## 변수들의 2차, 3차 항을 만들어낼 수 있다. 
## 이 경우 파생 변수를 추가해서 진행한다 ! 

# 3) 
# bmi 변수를 사용해서 overweight 파생 변수를 생성 
# bmi >= 30 이면 overweight = 1, bmi< 30 이면 overweight = 0
# expense ~ age + sex + ... + age_square + overweight 

insurance_mod$overweight <- ifelse(insurance_mod$bmi >= 30, 1, 0)
head(insurance_mod)

# insurance_mod <- insurance_mod %>% 
#   mutate(overweight = ifelse(insurance_mod$bmi >= 30, 1, 0))

str(insurance_mod)



X_train <- insurance_mod[1:train_size, ]
X_test <- insurance_mod[(train_size +1):1338, ]

lin_reg4 <- lm(formula = expenses ~ ., data = X_train)
lin_reg4
summary(lin_reg4)    #> Residual standard error: 5987
#> Multiple R-squared:  0.7548,	Adjusted R-squared:  0.7525

test_predicts4 <- predict(object = lin_reg4, newdata = X_test)
rmse(actual = X_test$expenses, predicted = test_predicts4) #> 5951.843

# 4) expenses ~ age + age_square + bmi + smoker + overweight + overweight * smoker 
lin_reg5 <- lm(formula = expenses ~ age + age_square + bmi + smoker + overweight + overweight * smoker,
               data = X_train)
lin_reg5
summary(lin_reg5)
#> Residual standard error: 4568
#> Multiple R-squared:  0.8567,	Adjusted R-squared:  0.8559 

test_predicts5 <- predict(object = lin_reg5, newdata = X_test)
rmse(actual = X_test$expenses, predicted = test_predicts5) #> 4400.227


