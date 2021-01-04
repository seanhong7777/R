# wisc_bc_data.csv : 위스콘신 대학 유방암 데이터프레임
# 암 데이터 분류 - knn 

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

# 3. knn 알고리즘 적용---
# knn()함수 사용해서 test set의 예측값 찾음
predicts <- knn(train_set, 
                test_set, 
                cl = train_label, 
                k = 5)

# 4. knn 성능 평가 ---
# 정확도 계산 
mean(predicts == test_label)
wrong_idx <- which(predicts != test_label)
test_label[wrong_idx]
predicts[wrong_idx]

# 오차 행렬() 
CrossTable(x = test_label, y = predicts, prop.chisq = FALSE)
#          : 예측값                :
# 실제값   :  Negative :  Positive :
# ---------+-----------+-----------+
# Negative :    TN     :     FP    :
# ---------+-----------+-----------+
# Positive :    FN     :     TP    :
# ---------+-----------+-----------+
tn <- 70 
fp <- 0
fn <- 4
tp <- 40

# 분류의 성능 지표(mettics)
# 1) 정확도(accuracy): 전체 sample 中 정확하게 예측한 비율
# accuracy = (TN + TP) / (TN + FP + FN + TP)
(tn + tp) / (tn + fp + fn + tp)

# 2) 정밀도(precision): Positive 예측들 중에서 정답 비율 
# precision = tp / (fp + tp)
precision <- tp / (fp + tp)
precision
# 3) 재현율(recall) : 실제 positive 샘플들 중 정답 비율
# recall = tp / (fn + tp)
# 재현율 = 민감도(sensitivy) = TPR (True Positive Rate, 진짜 양성 비율)
recall <- tp / (fn + tp)
recall
# 4) F1-score: 정밀도와 재현율의 조화 평균 (역수들의 평균의 역수) 
# f1 = 1 / ((1/precision + 1/recall) / 2) = 2 / (1/precision + 1/recall)
f1 = 2 / (1/precision + 1/recall)
f1

# 정밀도/재현율 trade-off (precision/reacall trade-off)
# 정밀도가 좋아지면 재현율이 나빠짐. 반대로, 재현율이 좋아지면 정밀도 나빠짐

# k = 1, 21 일때 결과 비교
predicts <- knn(train = train_set,
                test = test_set, 
                cl = train_label, 
                k = 21)
mean(predicts == test_label)
CrossTable(x = test_label, y = predicts, prop.chisq = FALSE)


cm_knn <- confusionMatrix(predicts, test_label, positive = "Malignant")

# 5. 모델 향상 ----
# 1) 변수 정규화(normalize) 
min_max_normalize <- function(x) {
  return ((x - min(x) / max(x) - min(x)))
}

# data set normalize
normalized_bc_data <- data.frame(lapply(bc_data, min_max_normalize))
summary(normalized_bc_data)

# split 
train_normalized <- normalized_bc_data[1:tr_size, ]
test_normalized <- normalized_bc_data[(tr_size+1):569, ]

norm_predicts <- knn(train = train_normalized,
                     test = test_normalized,
                     cl = train_label,
                     k = 5)
mean(norm_predicts == test_label)
CrossTable(x = test_label, y = norm_predicts, prop.chisq = FALSE)
# K=1: accuracy =   0.9473684 recall = 0.909
# K=5: accuracy =   0.9649123 recall = 0.909


# 2) 변수 표준화(standardization)
standardize <- function(x) {
  return((x - mean(x)) / sd(x))
}

# standarization
standardized_bc_data <- data.frame(lapply(bc_data, standardize))
summary(standardized_bc_data)


# split 
train_standardized <- standardized_bc_data[1:tr_size, ]
test_standardized <- standardized_bc_data[(tr_size +1):569, ]

stand_predicts <- knn(train = train_standardized,
                     test = test_standardized,
                     cl = train_label,
                     k = 5)
mean(stand_predicts == test_label)
CrossTable(x = test_label, y = stand_predicts, prop.chisq = FALSE)
# k=1: accuracy =  0.9561404  recall = 0.932
# K=5, accuracy =  0.9736842  recall = 0.955

