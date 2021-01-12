# Random Forst : Bagging + Decision Tree Ensemble 학습 방법
# Bagging(Bootstrap Aggregating): 
#  train set 에서 "중복을 허용(bootstrap)"하고 무작위로 sampling해서 
#  train set의 부분 집합을 만드는 방법
# Ensembl 학습 방법:
#  1) 여러개의 머신러닝 알고리즘을 하나의 train set에 학습시키는 방법 
#  2) 하나의 머신러닝 알고리즘을 여러개의 train set에서 학습시키는 방법

# install.packages('randomForest')
library(tidyverse)
library(randomForest)  # randomFores()
library(gmodels)       # CrossTable()
search()

# 1. Data import ---
iris <- datasets::iris
str(iris)

# 2. Train set(120)/Test set(80) split --- 
train_size <- 150 * 0.8 
set.seed(42)
idx <- sample(150)
idx
X_train <- iris[idx[1:train_size], ]
X_test <- iris[idx[(train_size+1):150], ]

table(X_train$Species)
table(X_test$Species)


# 3. Random Forest 알고리즘을 학습 --- 
forest_clf <- randomForest(formula = Species ~., data = X_train)
forest_clf
#> OOB estimate of  error rate: 5%

# Train set 평가
train_pred <- predict(object = forest_clf, newdata = X_train)
mean(X_train$Species == train_pred)    #> 100%
CrossTable(x = X_train$Species, y = train_pred, prop.chisq = FALSE)
# OOB estimate의 결과와 train set의 예측 결과는 다르다
# oob estimate의 결과는 test set의 예측 결과와 비슷 

# Test set 평가
test_pred <- predict(object = forest_clf, newdata = X_test)
mean(X_test$Species == test_pred)  #> 0.9333333
CrossTable(x = X_test$Species, y = test_pred, prop.chisq = FALSE)

# OOB(Out of Bagging) 
#  Bagging(Bootstrap Aggregating)에서 한번도 샘플링되지 않는 훈련 셋의 원소들
#  OOB Estimate: oob 샘플들을 사용해서 모델을 테스트하는 방법

# 4. Random Forest 분류 연습 
# wisc_bc_data.csv 데이터

wisc_bc <- read.csv(file = 'datasets/wisc_bc_data.csv', stringsAsFactors = TRUE)
str(wisc_bc)

# id 변수 제거
wisc_bc <- wisc_bc[, -1]
# diagnosis 변수를 factor 타입으로 변환 
wisc_bc$diagnosis <- factor(wisc_bc$diagnosis,
                            levels = c('B', 'M'),
                            labels = c('Benign', 'Malignant'))
str(wisc_bc)

# train set/test set = 8:2
N <- nrow(wisc_bc)
train_size <- round(N * 0.8)
X_train <- wisc_bc[1:train_size, ]
X_test <- wisc_bc[(train_size+1):N, ]

# Random Forest 모델 훈련
forest_clf <- randomForest(formula = diagnosis ~ ., data = X_train)
forest_clf
#>  OOB estimate of  error rate: 4.18%

# 훈련 셋  평가
train_pred <- predict(object = object = forest_clf, newdata = X_train)
CrossTable(x = X_train$diagnosis, y = train_pred, prop.chisq = FALSE)

# test set 평가
test_pred <- predict(object = forest_clf, newdata = X_test)
mean(X_test$diagnosis == test_pred)   #> 0.9736842
CrossTable (x = X_test$diagnosis, y = test_pred)

# credit.csv 데이터
credit <- read.csv(file = 'datasets/credit.csv', stringsAsFactors = TRUE)
str(credit)

X_train <- credit[1:800, ]
X_test <- credit[801:1000, ]

table(X_train$default)
table(X_test$default)

forest_clf <- randomForest(formula = default ~ ., data = X_train)
forest_clf
#> OOB estimate of  error rate: 24.5%

train_pred <- predict(object = forest_clf, newdata = X_train)
CrossTable(x = X_train$default, y = train_pred)

test_pred <- predict(object = forest_clf, newdata = X_test)
mean(X_test$default == test_pred)
CrossTable(x = X_test$default, y = test_pred)
