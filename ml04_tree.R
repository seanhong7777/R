# Decision tree 알고리즘을 사용한 대출 위험 예측 

library(tidyverse)
library(C50)
library(gmodels)
search()

# 1. 데이터 준비 --- 
# 독일 은행 대출 상환 정보
credit_df <- read.csv(file = 'datasets/credit.csv', encoding = 'UTF-8', stringsAsFactors = TRUE)

str(credit_df)
head(credit_df)
summary(credit_df)

# 목적: default(채무 불이행, 파산)여부 예측

# default 여부 시각화 
table(credit_df$default)
ggplot(data = credit_df) +
  geom_bar(aes(x = default))

# 2. 데이터 탐색 --- 
# checking_balance 별 default = yes의 개수 
credit_df %>% 
  filter(default == 'yes') %>% 
  count(checking_balance) %>% 
  ggplot() + geom_col(aes(x = checking_balance, y = n))

credit_df$savings_balance <- factor(credit_df$savings_balance,
                                    levels = c('< 100 DM', '100 - 500 DM', '500 - 1000 DM', '> 1000 DM', 'unknown'))

# saving_balance 별 default = yes 개수
credit_df %>% 
  filter (default == 'yes') %>% 
  count(savings_balance) %>% 
  ggplot() + geom_col(aes(x = savings_balance, y = n))

# amount(대출금) ~ age(나이) scatter plot
# 점의 색상을 default 여부에 따라서 다르게 시각화
ggplot(data = credit_df)+
  geom_point(aes(x = age, y = amount, color = default))

# amount(대출금) ~ months_loan_duration(대출기간) scatter plot
# 점의 색상을 default 여부에 따라서 다르게 시각화 
ggplot(data = credit_df)+
  geom_point(aes(x = months_loan_duration, y = amount, color = default))

# 3. 의사결정 나무 알고리즘 훈련 ---
# default 변수를 factor로 변환 
# credit_df$default <- factor(x = credit_df$default, 
#                             levels = c('no', 'yes'),
#                             labels = c('Default_No', 'Default_Yes'))

table(credit_df$default)

# 3. Decision Tree Training ---
# 훈련셋(9): 테스트셋(1) 분리. 훈련/테스트 레이블 분리
train_set <- credit_df[1:900, 1:16]
head(train_set)
train_label <- credit_df[1:900, 17]
table(train_label)
prop.table(table(train_label))

test_set <- credit_df[901:1000, 1:16]
test_label <- credit_df[901:1000, 17]
prop.table(table(test_label))


# C5.0() 함수 의사결정 나무를 생성
tree <- C5.0(x = train_set, y = train_label)
tree
summary(tree)

plot(tree)
plot(tree, subtree = 21)

# 훈련 셋의 정확도, 오차 행렬
train_predict <- predict(tree, train_set) 
mean(train_predict == train_label)   #> 학습셋 정확도= 87.6%
CrossTable(x = train_label, y = train_predict, prop.chisq = FALSE)

# 테스트 셋의 정확도, 오차 행렬 
test_predict <- predict(tree, test_set)
mean(test_predict == test_label)    # 테스트셋 정확도 = 67%  (과적합. 학습셋 대비 테스트셋 정확도 낮음)
CrossTable(x = test_label, y = test_predict, prop.chisq = FALSE)

# 과적합(overfitting)
# 머신러닝 알고리즘이 훈련 셋은 잘 설명하지만,
# 테스트셋을 잘 설명하지 못하는 경우 

## 5. 의샤결정나무 알고리즘 성능 개선 --- 
# 1) decision tree를 여러개를 만들어서, 다수결로 결정을 하는 방식 
tree2 <- C5.0(x = train_set, y = train_label, trials = 10)
tree2
summary(tree2)

# 훈련 셋의 예측
train_predict2 <- predict(tree2, train_set)

# 훈련 셋의 정확도 
mean(train_label == train_predict2)   #> 96.3%
CrossTable(x = train_label, y = train_predict2, prop.chisq = FALSE)

# 테스트 셋 예측
test_predict2 <- predict(tree2, test_set)

# 테스트 셋 정확도
mean(test_label == test_predict2)    #> 67% 
CrossTable(test_label, test_predict2, prop.chisq = FALSE)

# tree2는 tree1보다 훈련 셋에 더 많이 overfitting 

# 2) 오류(error)에 대한 비용 (손실, cost)의 가중치를 설정해서, 
# 비용이 큰 오류가 작아지도록 tree를 만드는 방법 
# default(파산) 여부인 경우에는 FP보다 FN가 비용(손실) 더 크 오류 
# -> FN이 개수가 줄어들도록 tree 보완 

# matrix(행렬)의 행과 열의 이름을 지정하기 위해서
cost_dim_names <- list(predict = c('no', 'yes'),   # row 이름
                       actual = c('no', 'yes'))    # column 이름
cost_dim_names

# 비용 행렬: 예측은 row, 실제는 column으로 설정
# 오차 행렬: 실제값을 row, 예측값을 column으로 보여줌 
cost_matrix <- matrix(data = c(0, 1, 2, 0),
                      nrow = 2, 
                      dimnames = cost_dim_names)
cost_matrix

# 비용(손실)을 고려한 의사결정 나무
tree3 <- C5.0(x = train_set, y = train_label, costs = cost_matrix)
tree3

train_predict3 <- predict(tree3, train_set)
mean(train_label == train_predict3)
# FP가 늘어나고, FN이 줄어듦 

test_predict3 <- predict(tree3, test_set)  # 테스트 셋 예측값
mean(test_label == test_predict3)          # 테스트 셋 정확도 59%
CrossTable(x = test_label, y = test_predict3, prop.chisq = FALSE)
