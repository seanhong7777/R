## 위스코신 암 데이터로 Decision tree 연습

# 0. ---
# 필요한 패키지들을 메모리에 로드
library(tidyverse)  # 데이터 전처리, 가공, 시각화, ... 
library(class)      # knn() 함수
library(gmodels)    # CrossTable() 함수 
search()

# 1. 데이터 준비 ---
# csv 파일을 읽어서 데이터프레임 생성
wisc_bc <- read.csv(file = 'datasets/wisc_bc_data.csv')
head(wisc_bc)
tail(wisc_bc)
str(wisc_bc)
summary(wisc_bc)

# 2. 데이터 전처리(가공) --- 
# id: 환자 아이디   -> 불필요
# diagnosis 변수: 암인지 아닌지가 진단된 데이터 (관심변수: label)
# knn() 함수에서 cl 파라미터에 전달하는 레이블은 factor 타입이어야 함
# -> diagnosis 변수를 factor 타입으로 변환 

table(wisc_bc$diagnosis)
# B: Benign    양성 종양(암 x)
# M: Malignant 악성 종양(암)

wisc_bc$diagnosis <- factor(x = wisc_bc$diagnosis, 
                            levels = c('B', 'M'),
                            labels = c('Benign', 'Malignant'))
str(wisc_bc$diagnosis)
table(wisc_bc$diagnosis)

# id 변수 제거, 데이터와 레이블 분리
bc_data <- wisc_bc[, 3:32]   #> data
bc_label <- wisc_bc[, 2]     #> label 


# dataset을 Training set 과 Label set 으로 분리
# Random하게 섞여 있어서 sample()함수를 사용하지 않음 
tr_size <- round(569 * 0.8) # train set의 observation 갯수
train_set <- bc_data[1:tr_size, ]   # 455 x 30 데이터프레임
train_label <- bc_label[1:tr_size]   # 455 1차원 벡터

test_set <- bc_data[(tr_size +1):569, ]  #> 114 x 30 데이터프레임
test_label <- bc_label[(tr_size + 1):569]

table(train_label)
prop.table(table(train_label))
table(test_label)
prop.table(table(test_label))



# C5.0() 함수 의사결정 나무를 생성
tree <- C5.0(x = train_set, y = train_label)
tree
summary(tree)

plot(tree)
#plot(tree, subtree = 21)

# 훈련 셋의 정확도, 오차 행렬
train_predict <- predict(tree, train_set) 
mean(train_predict == train_label)   #> 학습셋 정확도= 99.1%
CrossTable(x = train_label, y = train_predict, prop.chisq = FALSE)


# 테스트 셋의 정확도, 오차 행렬 
test_predict <- predict(tree, test_set)
mean(test_predict == test_label)    # 테스트셋 정확도 = 94.7%  
CrossTable(x = test_label, y = test_predict, prop.chisq = FALSE)
# Recall 90.9%


# 4. 평가
wrong_idx <- which(test_label != test_predict)
wrong_idx
test_set[wrong_idx, ]
test_label[wrong_idx]
