# ---
# KNN(K-Nearest Neighbor, k-최근접이웃) 알고리즘을 사용한 Iris(붓꽃) 품종 분류

# 필요한 라이브러리 설치 로딩
# install.packages('class') # classification(분류)
# install.packages('gmodels') # 혼동(오차) 행렬
library(tidyverse)
library(class)
library(gmodels)
search()

# 1. -----
# 데이터 셋 준비(datasets::iris 데이터 셋 복사)
?datasets::iris
iris <- as.data.frame(datasets::iris)
head(iris)
tail(iris)
str(iris)


# 2. -----
# 데이터 셋 탐색, 가공(전처리)
# iris 품종(species)의 분포
table(iris$Species)
iris %>% count(Species)

# 데이터 프레임에서 인덱스(row와 colum의 위치)로 원소를 선택 
iris[1, 1]
iris[1, 2]
iris[1, 3]
iris[1, 4]
iris[1, 5]
iris[1, ]    # iris 데이터프레임의 첫번째 row의 모든 컬럼
iris[1:5, ]  # iris 데이터프레임의 1~5 row의 모든 column 데이터 
iris[, 1:2]  # iris 데이터프레임의 1~2 column의 모든 row 데이터

# Petal.Length ~ Petal.Width 산점도

ggplot(data = iris) +
  geom_point(aes(x = Petal.Width, y = Petal.Length, color = Species))

qplot(data = iris, x = Petal.Width, y = Petal.Length, color = Species)

# Sepal.Length ~ Sepal.Width 산점도 그래프
# Species 따라서 점의 색상을 다르게 시각화
qplot(data = iris, x = Sepal.Width, y = Sepal.Length, color = Species)

# Species 별 4개 특징
ggplot(data = iris) +
  geom_boxplot(mapping = aes(x = Species, y = Sepal.Length))

ggplot(data = iris) +
  geom_boxplot(mapping = aes(x = Species, y = Sepal.Width))

ggplot(data = iris) +
  geom_boxplot(mapping = aes(x = Species, y = Petal.Length))

ggplot(data = iris) +
  geom_boxplot(mapping = aes(x = Species, y = Petal.Width))


# 3. -----
# 데이터 셋(150개 관찰값)을 훈련 셋(trining set)과 테스트 셋(test set)으로 분리
# 훈련 셋: 테스트 셋 = 100:50
# 데이터 프레임을 단순히 순서대로 100개, 50개 분리 -> 문제가 생김
# 데이터들이 섞여 있지 않기 때문에 virginica 품종은 훈련이 되지 않는 문제가 생김
# 데이터 셋을 분리하기 전에 무작위 섞어줘야 함 

sample(10)
# sample(n): 1~n까지 정수를 무작위 순서 섞인 벡터를 반환 
idx <- sample(10)
idx
idx[1:7]   # random하게 섞인 10개의 숫자들 중에서 앞에서 7개 선택 
idx[8:10]  # random하게 섞인 10개의 숫자들 중에서 뒤에서 3개 선택 

idx <- sample(150)
idx

train_set <- iris[idx[1:100], 1:4]
head(train_set)

train_label <- iris[idx[1:100], 5]
head(train_label)

test_set <- iris[idx[101:150], 1:4]
head(test_set)

test_label <- iris[idx[101:150], 5]
head(test_label)

# train_label의 품종별 빈도수
table(train_label)

# test_label의 품종별 빈도수 
table(test_label)

# 4. -----
# KNN 모델에 훈련셋/레이블, 테스트 셋을 적용해서 test set의 예측 값을 찾음 
test_predicts <- knn(train_set,        # 훈련 셋 
                     test_set,         # 테스트 셋
                     cl = train_label, # 훈련 셋의 레이블(class 약자)
                     k = 5)            # 가장 가까운 이웃을 몇 개 사용용
#> 테스트 셋의 예측값을 반환 
test_predicts
  
# 테스트 셋의 실제값
test_label 

# 예측값과 실제값을 비교 -> 평가(정확도 계산) 
test_predits == test_label
sum(test_predicts == test_label)  # > 예측이 맞은 개수 
# logical 값들 합: TRUE는 1, FALSE는 0으로 계산됨
mean(test_predicts == test_label)  # > sum / 전체 개수 : 정확도(accuracy)

# 예측값과 실제값이 다른 인덱스(위치) 
wrong_idx <- which(test_predicts != test_label)
wrong_idx
test_label[wrong_idx]  # 실제값: virginica
test_predicts[wrong_idx]  # 예측값: versicolor

# Confusion Matrix(혼동/오차 행렬)
CrossTable(x = test_label,      # X = 실제값
           y = test_predicts)   # y = 예측값


# 예측 결과 시각화
ggplot(data = test_set) +
  geom_point(mapping = aes(x = Petal.Width, y = Petal.Length, color = test_label))+
  geom_point(data = test_set[wrong_idx, ], 
             mapping = aes(x = Petal.Width, y = Petal.Length),
             shape ='x', size = 5, color = 'red')

ggplot(data = train_set) +
  geom_point(mapping = aes(x = Petal.Width, y = Petal.Length, color = train_label))+
  geom_point(data = test_set[wrong_idx, ], 
             mapping = aes(x = Petal.Width, y = Petal.Length),
             shape ='x', size = 5, color = 'red')


# 5. -----
# 특성 스케일링(feature scaling): 정규화(normalization), 표준화(standardization)
# 변수(특성)들마다 단위, 크기가 달라서, 거리를 계산할 때 미치는 영향이 다를 수 있음 
# 모든 변수(특성)들이 거리 계산을 할 때 비슷한 영향을 미칠 수 있도록 scale을 변환
# 정규화: 
#   변수의 최솟값을 0, 최댓값을 1로 . 모든 값들이 0~1 범위가 되도록 스케일링
# 표준화:
#   변수의 평균이 0, 표준편차가 1이 되도록 스케일링
summary(iris)

v1 <- seq(1 ,5)
v1
min_v1 <- min(v1)
min_v1
max_v1 <- max(v1)
max_v1
v1_normalized <- (v1 - min_v1) / (max_v1 - min_v1)
v1_normalized

v1_normalized <- (v1 -  min(v1)) / (max(v1) - min(v1))
v1_normalized

v2 <- seq(10, 50, 10)
v2
v2_normalized <- (v2 -  min(v2)) / (max(v2) - min(v2))
v2_normalized


# 함수 정의(definition)/선언(declaration)
# 함수이름  <-  function(파라미터 선언) {함수 기능 작성}
min_max_normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# 함수는 호출(call)해야 기능이 수행됨
min_max_normalize(v1)
min_max_normalize(v2)

df <- data.frame(width = v1, length = v2)
df
summary(df)

# lapply(데이터프레임, 함수이름)
#  데이터프레임의 변수들을 하나씩 함수의 argument로 전달 
#  함수의 리턴값들을 list로 묶어서 리턴(반환)
df_normalized <- lapply(df, min_max_normalize)
df_normalized   #> 리스트(list)
df_normalized <- data.frame(df_normalized)
df_normalized

# min_max_normalize 함수는 숫자 타입 변수만 argument로 전달받을 수 있기 때문에 
# iris 데이터프레임을 수자 타입 변수들과 label을 분리 
iris_data <- iris[, 1:4]
head(iris_data)

iris_label <- iris[, 5]
head(iris_label)

iris_data_norm <- data.frame(lapply(iris_data, min_max_normalize))
summary(iris_data_norm)
head(iris_data_norm)

# 정규화된 데이터프레임을 훈련 셋/테스트 셋으로 분리
idx # 1 ~ 150 정수들이 랜덤하게 섞여있는 벡터 

# 훈련 셋, 훈련 레이블
normalized_train_set <- iris_data_norm[idx[1:100], ]
train_label <- iris_label[idx[1:100]]

# 테스트 셋, 테스트 레이블
normalized_test_set <- iris_data_norm[idx[101:150], ]
test_label <- iris_label[idx[101:150],]

# knn 알고리즘 적용
predicts <- knn(train = normalized_train_set,
                test = normalized_test_set,
                cl = train_label,
                k =11)

predicts == test_label
mean(predicts == test_label)  # 정확도(accuracy)
CrossTable(x = test_label, y = predicts)

#표준화 
standardize <- function(x) {
  # 변수 x의 평균을 0, 표준편차를 1로 변환하는 스케일링 방법
  return((x - mean(x)) / sd(x))
}

v1 
mean(v1)
sd(v1)
v1_stand <- standardize(v1)
v1_stand
mean(v1_stand)
sd(v1_stand)

# standardize 함수를 iris 데이터프레임에 적용
summary(iris_data)
iris_data_stand <- data.frame(lapply(iris_data, standardize))
summary(iris_data_stand)
sd(iris_data_stand$Sepal.Length)

# 표준화를 적용한 데이터프레임을 훈련/테스트 셋으로 분리
stand_train_set <- iris_data_stand[idx[1:100], ]
stand_test_set <- iris_data_stand[idx[101:150], ]

# knn 적용 -> 예측값
predicts <- knn(train = stand_train_set, 
                test = stand_test_set,
                cl = train_label,
                k = 11)

# 정확도
mean(predicts == test_label)
# 오차 행렬
CrossTable(x = test_label, y = predicts)

           