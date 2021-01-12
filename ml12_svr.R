# SVM(Support Vector Machine) Regression(회귀) - 수치 예측

# 패키지를 메모리에 로드
library(tidyverse)
library(kernlab)        # ksvm()
library(ModelMetrics)   # mse(), rmse(), mae()
library(psych)          # pairs.panel()
library(rpart)
library(rpart.plot)
search()

# 1. 데이터 준비 ---
# SVM을 사용한 콘크리트 강고(strength) 예측
concrete <- read.csv(file ='datasets/concrete.csv')
head(concrete)
str(concrete)
# strength : 콘크리트 강도. 관심(종속) 변수 
# strength ~ 

# 2. 데이터 탐색 --- 
pairs.panels(concrete)

# 3. SVM 회귀 모델 학습 ---
# 훈련 셋/테스트 셋 분리 
N <- nrow(concrete)
train_size <- round(N * 0.8)
X_train <- concrete[1:train_size, ]
X_test <- concrete[(train_size + 1):N, ]

summary(X_train$strength)
summary(X_test$strength)

# SVM 예측기(regressor)를 훈련 셋으로 학습시킴
svm_reg <- ksvm(x = strength ~ ., data = X_train, kernel = 'vanilladot')
svm_reg

# 훈련 셋 평가
train_pred <- predict(object = svm_reg, newdata = X_train)
train_pred[1:10]
X_train$strength[1:10]
rmse(actual = X_train$strength, predicted = train_pred) #> 10.44347

# 4. Test set으로 모델 평가 ---
test_pred <- predict(object = svm_reg, newdata = X_test)
rmse(actual = X_test$strength, predicted = test_pred)    #> 11.8534

# 5. kernel 변경에 따른 효과 --- 
# 1) GAussian RBF 커널: 
svr_rbf <- ksvm(x = strength ~ ., data = X_train, kernel = 'rbfdot')
svr_rbf
rbf_pred <- predict(object = svr_rbf, newdata = X_test)
summary(X_test$strength - rbf_pred)   # residuals 분포
rmse(actual = X_test$strength, predicted = rbf_pred)     #> 6.904135

# 2) Polynomial 커널: 
svr_poly <- ksvm(x = strength ~., data = X_train, kernel = 'polydot',
                 kpar =list(degree = 3))
svr_poly
poly_pred <- predict(object = svr_poly, newdata = X_test)
summary(X_test$strength - poly_pred)      # residuals 분포 
rmse(actual = X_test$strength, predicted = poly_pred)    #> 6.503291

# 6. SVM 이외의 ML 알고리즘과 비교 ---
# lm(): Linear Regression, rpart(): Regression Tree 

lin_reg <- lm(formula = strength ~., data = X_train)
test_pred <- predict(object = lin_reg, newdata = X_test)
rmse(actual = X_test$strength, predicted = test_pred)   #> 11.37807

reg_tree <- rpart(formula = strength ~., data = X_train)
test_pred2 <- predict(object = reg_tree, newdata = X_test)
rmse(actual = X_test$strength, predicted = test_pred2)  #> 9.169914


# 7. 의료비 지출(insurance.csv) 데이터 프레임을 사용해서
# SVR, Linear Regression, Regression Tree의 결과를 비교해 보세요
insurance <- read.csv(file = 'datasets/insurance.csv', stringsAsFactors = TRUE)

head(insurance)
str(insurance)

set.seed(10)

N <- nrow(insurance)
idx <- sample(N)

train_size <- round(N * 0.8)
X_train <- insurance[idx[1:train_size], ]
X_test <- insurance[idx[(train_size + 1):N], ]
summary(X_train$expenses)
summary(X_test$expenses)

# 1) SVR
svr_reg <- ksvm(x = expenses ~., data = X_train)
svr_pred <- predict(object = svr_reg, newdata = X_test)
summary(X_test$expenses - svr_pred)
rmse(actual = X_test$expenses, predicted = svr_pred)   #> 5212.972

# 1-2) Gaussian RBF 커널: 
svr_rbf <- ksvm(x = expenses ~ ., data = X_train, kernel = 'rbfdot')
rbf_pred <- predict(object = svr_rbf, newdata = X_test)
summary(X_test$expenses - rbf_pred)   # residuals 분포
rmse(actual = X_test$expenses, predicted = rbf_pred)     #> 5215.366

# 1-3) Polynomial 커널: 
svr_poly <- ksvm(x = expenses ~., data = X_train, kernel = 'polydot',
                 kpar =list(degree = 3))
poly_pred <- predict(object = svr_poly, newdata = X_test)
summary(X_test$expenses - poly_pred)      # residuals 분포 
rmse(actual = X_test$expenses, predicted = poly_pred)    #> 5134.891

# 2) Linear Regression
lin_reg <- lm(formula = expenses ~., data = X_train)
lin_pred <- predict(object = lin_reg, newdata = X_test)
summary(X_test$expenses - lin_pred)
rmse(actual = X_test$expenses, predicted = lin_pred)   #> 6202.026

# 3) Regression Tree
reg_tree <- rpart(formula = expenses~., data = X_train)
reg_pred <- predict(object = reg_tree, newdata = X_test)
summary(X_test$expenses - reg_pred)
rmse(actual = X_test$expenses, predicted = reg_pred)   #> 5374.613
rpart.plot(reg_tree)


