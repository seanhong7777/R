# SVM(Support Vector Machine)를 사용한 암(cancer) 예측 

# 1. 데이터 준비 ---
# wisc_bc_data.csv
wisc_bc <- read.csv (file = 'datasets/wisc_bc_data.csv', stringsAsFactors = TRUE)
head(wisc_bc)
str(wisc_bc)

# 2. 데이터 탐색, 전처리, 가공 --- 
# id 제거
df <- wisc_bc[, -1]
# diagnosis 변수를 factor 타입으로 변환
df$diagnosis <- factor(df$diagnosis,
                       levels = c('B', 'M'),
                       labels = c('Benign', 'Malignant'))
str(df)
summary(df$diagnosis)

# 3. SVM classifier 분류기를 학습 --- 
# train set / test set = 8:2 
N <- nrow(df)
test_size <- round(N * 0.8)
X_train <- df[1:test_size, ]
X_test <- df[(test_size +1):N, ]

prop.table(table(X_train$diagnosis))
prop.table(table(X_test$diagnosis))

# Train set으로 학습시킴
svm_clf <- ksvm(x = diagnosis ~ ., data = X_train, kernel = 'vanilladot')
svm_clf

# train set에서 정확도, 오차 행렬 
train_pred <- predict(object = svm_clf, newdata = X_train) 
mean(X_train$diagnosis == train_pred)    #> 0.989011
CrossTable(x = X_train$diagnosis, y = train_pred, prop.chisq = FALSE) #> 0.970

# 4. SVM 분류기 평가 ---
# 테스트 셋에서 정확도, 오차 행렬
test_pred <- predict(object = svm_clf, newdata = X_test)
mean(X_test$diagnosis == test_pred)      #> 0.9824561
CrossTable(x = X_test$diagnosis, y = test_pred, prop.chisq = FALSE)   #> 0.955  

# 5. SVM 분류기를 여러 가지 kernel 에서 비교 테스트 
# rbfdot
svm_clf2 <- ksvm(x = diagnosis ~ .,
                 data = train_set,
                 kernel = 'rbfdot')

train_pred2 <- predict(object = svm_clf2, newdata = train_set)
mean(train_set$diagnosis == train_pred2)   #> 0.9846154
CrossTable(x = train_set$diagnosis, y = train_pred2) #> Recall : 0.958 

test_pred2 <- predict(object = svm_clf2, newdata = test_set)
mean(test_set$diagnosis == test_pred2)     #> 1
CrossTable(x = test_set$diagnosis, y = test_pred2) #> Recall : 1 

# polydot
svm_clf3 <- ksvm(x = diagnosis ~ .,
                 data = train_set,
                 kernel = 'polydot')

train_pred3 <- predict(object = svm_clf3, newdata = train_set)
mean(train_set$diagnosis == train_pred3)   #> 0.989011
CrossTable(x = train_set$diagnosis, y = train_pred3) #> Recall : 0.970 

test_pred3 <- predict(object = svm_clf3, newdata = test_set)
mean(test_set$diagnosis == test_pred3)     #> 0.9823009
CrossTable(x = test_set$diagnosis, y = test_pred3)   #> Recall : 0.953 
