# SVM(Support Vector Machine) Classification (분류) 1/8

# install.packages('kernlab')  # kernel SVM 알고리즘을 구현한 패키지 

library(tidyverse)    # 데이터 전처리, 가공, 시각화
library(kernlab)      # kernel SVM을 구현한 함수 사용. ksvm()
library(gmodels)      # CrossTable() - confusion matrix 
search() 

# 1. 데이터 준비 --- 
iris <- datasets::iris
str(iris)
head(iris)
tail(iris)

# 2. 데이터 탐색 --- 
# Petal.Length ~ Petal.Width scatter plot
# Species 따라서 점의 색상을 다르게 시각화
ggplot(data = iris)+
  geom_point(aes(x = Petal.Width, y = Petal.Length, color = Species))

# 3. SVM 알고리즘 학습 --- 
# train set / test set split - random sampling 8:2 

set.seed(42)
idx <- sample(150)
idx

iris_train <- iris[idx[1:120], ]
iris_test <- iris[idx[121:150], ]

head(iris_train)
head(iris_test)

# 붗꽃 세가지 품종이 골고루 섞여 있는지 확인 
prop.table(table(iris_train$Species))
prop.table(table(iris_test$Species))

# SVM 모델을 훈련 셋으로 학습시킴 
svm_clf <- ksvm(x = Species ~ .,       # x = formula(종속변수 ~ .)
                data = iris_train,     # data = (훈련셋) 데이터프레임 
                kernel = 'vanilladot') # kernel = 'vanilladot': 선형 커널 
svm_clf

# train set 예측 결과
train_pred <- predict(object = svm_clf, newdata = iris_train)
mean(iris_train$Species == train_pred)  #> 0.9833333
CrossTable(x = iris_train$Species, y = train_pred, prop.chisq = FALSE)

# 4. SVM 모델 평가  ---
test_pred <- predict(object = svm_clf, newdata = iris_test)
mean(iris_test$Species == test_pred)
CrossTable(x = iris_test$Species, y = test_pred, prop.chisq = FALSE )

# 5. SVM 모델에서 kernel 선택 비교 --- 
# Gaussian RBF
svm_rbf <- ksvm(x = Species ~ ., 
                data = iris_train, 
                kernel = 'rbfdot')
svm_rbf     #> 0.016667 
train_pred2 <- predict(object = svm_rbf, newdata = iris_train)
CrossTable(x = iris_train$Species, y = train_pred2)

test_pred2 <- predict(object = svm_rbf, newdata = iris_test)
mean(iris_test$Species == test_pred2)  #> 0.9666667
CrossTable(x = iris_test$Species, y = test_pred2, prop.chisq = FALSE)

# Polynomial kernel 
svm_poly <- ksvm(x = Species ~ .,
                 data = iris_train,
                 kernel = 'polydot')
svm_poly     #> 0.016667 
train_pred3 <- predict(object = svm_poly, newdata = iris_train)
CrossTable(x = iris_train$Species, train_pred3)

test_pred3 <- predict(object = svm_poly, newdata = iris_test)
mean(iris_test$Species == test_pred3)     #> 0.9666667
CrossTable(x = iris_test$Species, y = test_pred3, prop.chisq = FALSE)
#> polynomal kernel 인데 degree = 1은 linear kernel과 같은 결과 
