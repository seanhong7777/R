# Linear Regression 

library(tidyverse)
search()

# import dataframe
heights_df <- read.csv(file = 'datasets/heights.csv')
str(heights_df)
summary(heights_df)
head(heights_df)

# Dataframe exploration
# Visualization on father vs. son
ggplot(data = heights_df)+
  geom_boxplot(aes(y = father))

ggplot(data= heights_df)+
  geom_histogram(aes(x = father),
                 bins = 10,
                 color = 'black', 
                 fill = 'lightgray')

# Son
ggplot(data = heights_df)+
  geom_boxplot(aes(y = son))

ggplot(data =heights_df)+
  geom_histogram(aes(x = son), bins = 10, color = 'black', fill = 'lightgray')

# son ~ father scatter plot
ggplot(data = heights_df, aes(x = father, y = son))+
  geom_point(color = 'darkgray') +
  geom_smooth(method = 'lm')
# lm : linear modeling ; linear regression 
# y(dependent) ~ x(indenpendent)의 linear 관계를 찾는 것 
# y = a + b * x 
# 선형 회귀의 목적: 
#   실제 데이터(x_i, y_i) 
#   예측 데이터(x_i, y_hat_i), y_hat_i = a + b * x_i
#   오차(error) = 실제값 - 예측값: e_i = y_i - y_hat_i 
#   오차 제곱들의 합을 최소화하는 절편 a와 기울기 b를 찾는 것 
#   sum(e_i ^ 2)을 최소화 

# N: 변수 x의 개수 
# x_bar: 변수 x의 평균
# 분산(variance) : 데이터가 평균으로부터 떨어져 있는 정도 표현하는 값
# variance = sum((x_i - x_bar) ^ 2) / (N -1)
var(heights_df$father) #> 아버지 키의 분산 = 48.61361

N <- 1078
father_bar <- sum(heights_df$father) / N
mean(heights_df$father)

father_variance <- sum((heights_df$father - father_bar) ^2) / (N-1)

# 표준편차(standard deviation) = sqar(variance)
father_std <- sqrt(father_variance)
sd(heights_df$father)

# 두 변수 x, y의 공분산: 
# Covariance(x, y) = sum((x_i - x_bar)*(y_i - y-bar)) / (N-1)
# father, son의 공분산
son_bar <- mean(heights_df$son)   # son의 평균
covariance <- sum((heights_df$father - father_bar) * (heights_df$son - son_bar)) / (N-1)             
cov(x = heights_df$father, y = heights_df$son)  #> 24.98318

# R 통계 함수: mean(평균), var(분산), sd(표준편차), cov(공분산)

# OLS (Ordinary Least Squares)
# 선형 회귀식 y = a + b*x에서 변수 x의 기울기 b는 다음과 같이 계산됨됨
# b = Cov(x, y) / var(x)
b = cov(heights_df$father, heights_df$son) / var(heights_df$father)

# 선형회귀식 y = a + b*x로 만들어지는 직선은 두 변수 x, y의 평균을 지남 
# y_bar = a + b * x_bar
# a = y_bar - b * x_bar
a <- mean(heights_df$son) - b * mean(heights_df$father)

# lm() 함수: 선형 회귀식의 계수들(y절편, 기울기)을 찾아주는 함수
lin_reg <- lm(formula = son ~ father, data = heights_df)
lin_reg
lin_reg$coefficients  #> intercept, 기울기 
lin_reg$coefficients[1] #> y intercept 
lin_reg$coefficients[2] #> 변수 father의 기울기 

ggplot(data = heights_df,aes(x = father, y = son))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = 'lm')+
  geom_vline(xintercept = mean(heights_df$father), color = 'darkgreen', size = 1, linetype = 'dashed') +
  geom_hline(yintercept = mean(heights_df$son), color = ' darkgreen', size = 1, linetype = 'dashed') +
  geom_abline(slope = b, intercept = a, color = 'red', linetype = 'dotted')

# 선형 회귀 방정식 y_hat = a + b * x의 예측값은 오차들의 제곱의 합을 최소화
y <- heights_df$son     # 아들 키 : 종속 변수
x <- heights_df$father  # 아버지 키 : 독립 변수 
# 선형 모델(lm)에서 예측한 아들 키 
y_hat <- a + b * x 
# 오차(=실제값-예측값)들의 제곱의 합계
sum((y - y_hat) ^2)     #> 41242.11

# lm 모형과 다른 모형
y_hat2 <- 85 + 0.6 * x 
sum((y - y_hat2) ^ 2)   #> 243898

# 회귀(regression: 수치 예측)의 성능 지표:
# MSE(Mean Square Erro) : 오차들의 제곱의 평균 
# RMSE(Root Mean Square Erros): 오차들의 제곱의 평균의 제곱근 
# MAE(Mean Absolute Errors): 오차들의 절대값의 평균균

# lm에서 MSE 
mse_son <-mean((y - y_hat) ^ 2)  #> 38.25799
rmse_son <- sqrt(mse_son)        #> 6.1853
rmse_son
mae_son <- mean(abs(y - y_hat))
mae_son
# RMSE, MAE: 회귀 모델이 예측한 값들이 실제값과 평균적으로 어느정도 오차가 발생하는지를 설명

# 성능 지표를 계산해 주는 함수를 가지고 있는 패키지 설치 
# install.packages('ModelMetrics')
library(ModelMetrics)
mse(y, y_hat)
rmse(y, y_hat)
mae(y, y_hat)
