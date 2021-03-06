# 의사결정 나무(decision tree) 알고리즘을 사용한 붗꽃 분류(classification)

# --- 
# 필요한 라이브러리 설치 및 메모리 로드 
# install.packages('C50')
# C5.0 알고리즘을 사용해서 Decision Tree를 구현한 패키지 

library(tidyverse)
library(C50)
library(gmodels)
search()



# 1. 데이터 준비 --- 
# 붓꽃(iris) 품종(Species) 
iris <- datasets::iris
str(iris)
head(iris)
summary(iris)


# 2. 데이터 탐색, 전처리(가공) ---
# Petal.Length ~ Petal.Width 산점도 그래프(scatter plot)
# 점의 색상은 Species에 따라서 다르게 시각화

ggplot(data = iris)+
  geom_point(mapping = aes(x = Petal.Width, y = Petal.Length, color = Species))+
  geom_hline(yintercept = 2.0, linetype = 'dashed')+
  geom_vline(xintercept = 1.7, linetype = 'dashed')


# iris 데이터프레임을 데이터/레이블 분리 
iris_data <- iris[, 1:4]
head(iris_data)
iris_label <- iris[, 5]
head(iris_label)

# 데이터/레이블을 훈련/테스트 셋으로 분리 
# 데이터프레임을 랜덤하게 섞어서 훈련/테스트 셋으로 분리
set.seed(1)   # 난수(random nuber)를 만드는 순서를 고정 
idx <- sample(150)
idx

# 훈련:테스트 = 8:2 = 120:30 
train_idx <- idx[1:120]
test_idx <- idx[121:150]

train_set <- iris_data[train_idx, ]
train_label <- iris_label[train_idx]

# 테스트 셋, 테스트 레이블
test_set <- iris_data[test_idx, ]
test_label <- iris_label[test_idx]

# 훈련/테스트 셋이 편향되지 않았는지 체크 
table(train_label)
prop.table(table(train_label))
prop.table(table(test_label))

# 3. 머신러닝 모델(알고리즘) 적용, 평가 ---
# 의사 결정 나무 머신러닝 모델을 훈련(학습)시킴
tree <- C5.0(x = train_set,    # x = 훈련 셋(데이터프레임)
             y = train_label)  # y = 훈련 레이블(factor 벡터)
tree
summary(tree)

# C5.0 Decision tree 시각화
plot(tree)

# Decision tree 모델은 knn과 다르게 scaling 적용 불필요 

# 학습 셋의 정확도 
train_predict <- predict(tree, train_set)
train_predict   # 의사결정나무가 예측한 학습 셋의 예측값
train_label     # 학습 셋의 실제값
mean(train_label == train_predict)  # 훈련 셋의 정확도(accuracy)

CrossTable(x = train_label, y = train_predict, prop.chisq = FALSE)
#> 오차 행렬- summary(tree)의 내용과 동일 

# 테스트 셋으로 decision tree 알고리즘 평가 
# 테스트 셋의 예측값
test_predict <- predict(tree, test_set)
# 테스트 셋의 정확도
mean(test_label == test_predict)  #> 96.7%

# 테스트 셋에서 예측이 틀린 샘플듯
wrong_idx <- which(test_label != test_predict)
wrong_idx
test_set[wrong_idx, ]
test_predict[wrong_idx]  #> 예측: versicolor
test_label[wrong_idx]

# 테스트셋의 오차행렬
CrossTable(x = test_label, y = test_predict, prop.chisq = FALSE)
