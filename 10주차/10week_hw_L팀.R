# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("psych")
# install.packages("caret")
# install.packages("e1071")
# install.packages("DAAG")
# install.packages("car")
# install.packages("PerformanceAnalytics")
# install.packages("olsrr")
# install.packages("Metrics")
# install.packages("corrplot")

library(dplyr)    # tidyverse (tibble ..)
library(ggplot2)  # graph
library(psych)    # 기술 통계 (describe())
library(caret)    # modeling, training
library(e1071)    # density plot, skewness(linearMode$residuals)

library(DAAG)  # 선형 회귀 모델 k-Fold Cross-Validation
library(car)   # 설명력 분석 (test colinearity, vif(model))
library(PerformanceAnalytics)  # 상관관계 분석 (chart.Correlation(df))  
library(olsrr) # 변수 선택 (선택 사항 1번)
library(Metrics)  # RMSE 메소드
library(corrplot) # 상관계수 시각화 툴

getwd()
data <- read.csv (
  file = "gt_2011.csv",
  header = TRUE
)


######################### 데이터 EDA 및 전처리 ##########################

# 간단한 EDA (변수 11개, 레코드 7411개, 목표변수는 NOX(배출 가스 내 질소 산화물량)로 선정)
# 변수 타입은 모두 Numeric
head(data)
str(data)
describe(data)

# 목표변수를 NOX로 선정하였으므로 데이터의 활용 목적 상 CO 변수는 제거
data <- data[, -10]

# 데이터 전처리
# 1. 결측치 확인 (확인 결과 결측치 X)
colSums(is.na(data))

# 2. 극단값 처리
# AT (극단값 없음)
boxplot(data$AT)$stats

# AP (48개 삭제)
boxplot(data$AP)$stats
data$AP <- ifelse(data$AP < 997.39 | data$AP > 1030.50, NA, data$AP)
table(is.na(data$AP))
data <- data %>% filter(!is.na(AP))

# AH (32개 삭제)
boxplot(data$AH)$stats
data$AH <- ifelse(data$AH < 41.2550 | data$AH > 100.1700, NA, data$AH)
table(is.na(data$AH))
data <- data %>% filter(!is.na(AH))

# AFDP (25개 삭제)
boxplot(data$AFDP)$stats
data$AFDP <- ifelse(data$AFDP < 2.75840 | data$AFDP > 5.73580, NA, data$AFDP)
table(is.na(data$AFDP))
data <- data %>% filter(!is.na(AFDP))

# GTEP (극단값 없음)
boxplot(data$GTEP)$stats

# TIT (439개 삭제)
boxplot(data$TIT)$stats
data$TIT <- ifelse(data$TIT < 1056.2 | data$TIT > 1100.6, NA, data$TIT)
table(is.na(data$TIT))
data <- data %>% filter(!is.na(TIT))

# TAT (24개 삭제)
boxplot(data$TAT)$stats
data$TAT <- ifelse(data$TAT < 521.200 | data$TAT > 550.610, NA, data$TAT)
table(is.na(data$TAT))
data <- data %>% filter(!is.na(TAT))

# CDP (결측치 없음)
boxplot(data$CDP)$stats

# TEY (결측치 없음)
boxplot(data$TEY)$stats

# 3. 전처리된 데이터 EDA (레코드: 7411개 --> 6843개, 변수: 11개 --> 10개)
head(data)
str(data)
describe(data)

# 예측-목표 변수들 간의 관계 파악 (상관계수, 선형관계)
# 정규분포 (Normality) 근접한지 확인 (정규 분포가 선호됨)
windows()
chart.Correlation(data, historam=TRUE)

# 상관계수 시각화
corr <- cor(data)
corrplot(corr,method="ellipse")

# 분석 결과 (예측-목표 변수 간의 관계)
# 1. 선형 관계: AT, AP, AH 이 선형 관계 보임
# 2. 상관관계: AT(-0.70), AP(0.39), AH(0.20), TAT(0.19)가 비교적 상관관계를 보임
# 3. 분포 형태: AP가 정규 분포 형태를 가짐
#               AH, TAT는 한쪽으로 치우쳐진 상태의 정규 분포 형태를 가짐


###################### 예측 모델, 설명 모델 생성 ##########################

# 예측 목적의 모델: lm_pred  //  설명 목적의 모델: lm_desc

# 1. full model로 예측 목적의 모델 생성
# 전체 데이터 셋을 tr, ts set으로 분할하여 학습용 데이터 셋인 tr set으로 학습
set.seed(1)
tr_idx <- sample(1:nrow(data), 0.8*nrow(data))
tr_set <- data[tr_idx, ]
ts_set  <- data[-tr_idx, ]

lm_pred <- lm(NOX ~ ., data=tr_set)
lm_pred

# 2. full model로 설명 목적의 모델 생성
# 설명 목적의 모델은 전체 데이터 셋을 활용
lm_desc <- lm(NOX ~ ., data=data)
lm_desc

###################### 예측 모델 성능 평가 ##########################

# 예측 모델의 성능은 ts set 즉, 테스트용 데이터 셋에 대해 측정
pred <- predict(lm_pred, ts_set)
actuals_preds <- data.frame(cbind(actuals=ts_set$NOX, predicteds=pred))


# 예측 모델로써의 성능은 Min-Max Accuracy, MAPE, MSE, RMSE로 측정

# 1. 실제 목표변수 값과 예측 값의 상관계수 ==> 0.8680935
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# 2. Min-Max Accuracy (실제값과 예측값이 얼마나 근접한가) ==> 0.958693 (약 95.87%)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

# 3. MAPE (오차와 실제값의 비율)  ==> 0.0435968 (약 4.36%)
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

# 4. MSE (테스트 사례들에 대한 에러 자승의 평균)
# MSE는 CV(교차검증)을 통해 측정 (data는 예측 모델 생성에 활용한 tr_Set 적용)
# 측정결과 MSE ==> 10227.17
windows()
cvResults <- suppressWarnings(
  CVlm(data = tr_set, 
       form.lm=NOX ~ .,
       m=5, 
       dots=FALSE, 
       seed=29, 
       legend.pos="topleft",  
       printit=FALSE, 
       main="Small symbols are predicted values while bigger ones are actuals.")); 

mse <- attr(cvResults, 'ms')
mse

# 5. RMSE (MSE에 루트를 취한 값) rmse() 함수 말고도 sqrt 함수 (루트)를 사용해도 같음
# 측정결과 RMSE ==> 101.1295
rmse <- rmse(cvResults$NOX, cvResults$cvpred)
rmse


###################### 설명 모델 성능 평가 ##########################

# 설명 모델의 성능은 전체 데이터 셋에 대하여 측정

# 설명 모델로써의 성능을 측정하는데 테스트 집합이 필요없는 이유
# >>
# 설명 모델에는 테스트 집합이 필요 없다
# 설명 모델의 목적은 데이터를 설명하는 것으로,
# 모델이나 변수가 얼마나 정확하게 예측 변수와 목표 변수의 관계를 설명하는 가를
# 알아내는 것이 목적으로, 즉 성능 척도가 모델의 설명력과 유의미성 (significance)이다.
# 따라서 테스트 집합을 따로 나눌 필요없이 데이터 전체를 모델에 적용하는 것이
# 분석할 수 있는 데이터의 양이 더 많으므로 관계를 파악하는데 유리하다.

summary(lm_desc)

# 1. 모델의 설명력: Multiple R-squared & Adjusted R-squared
# Multiple R-squared: 0.7578 (약 0.76)
# Adjusted R-squared: 0.7575 (약 0.76)
# ==> 약 0.76

# 2. 모델의 유의미성: F-statistic 의 p-value
# F-statistic:  2376 on 9 and 6833 DF,  p-value: < 2.2e-16
# F-statistic 의 p-value 가 유의 수준 2.2e-16 보다 작음 ==> 모델이 유의미하다.


# 설명 모델의 성능과 유의미성 시험 끝

#####################################################
# 변수 별 목표변수에 영향을 미치는 강도 측정

summary(lm_desc)

# 변수에 대한 T-test 의 p-value 를 기준으로 함 (Pr(>|t|)) (강도가 높은 순)
# >>
# AT: < 2e-16
# AH: < 2e-16
# AFDP: < 2e-16
# TIT: < 2e-16
# TEY: < 2e-16
# AP: 4.52e-07
# GTEP: 0.000153
# TAT: 0.000809
# CDP: 0.004674

# 모든 예측 변수가 유의미 (CDP가 상대적으로 p-value가 높으나 이 변수도 매우 유의미함)
# AT, AH, AFDP, TIT, TEY 가 가장 유의미

######################################################
# 각 변수의 회귀계수는 어떠한 의미를 가지는가?

lm_desc$coefficients

######################################################
# (다중) 공선성 진단

# 분산팽창계수(VIF, Variance Inflation Factor)를 구하여 판단
# 엄밀한 기준은 없으나 보통 10보다 크면 다중 공선성이 있다고 판단
# (5를 기준으로 하기도 함)

vif(lm_desc)

# AP (2.4891), AH(1.7904), AFDP(6.0765)로 다중 공선성 x

# 다음 변수들은 다중 공선성이 있다고 판단됨
# AT (16.5010)
# GTEP (497.2700), TIT (359.3300), TAT (282.2600)
# TEY (423.6500), CDP (279.4300)


############ 변수 선택 (Stepwise-selection 진행) ###############
# 다중 공선성을 해결함과 동시에 성능을 최적화하기 위한 변수 선택 진행

# 변수 선택
# 1. forward selection
# 2. backward elimination
# 3. stepwise selection (hybrid)

# 모델링
model <- lm(NOX ~ ., data = data)

# 1. forward selection
# 선택된 변수: AT, TAT, AH, TIT, TEY, AFDP, AP, GTEP, CDP 순서로 선택
# 함수에서 9가지 변수를 모두 선택

# 그래프 분석 결과: 6개 선택 이후로는 거의 미미한 차이만이 있음
# 하지만 미미한 차이로 계속 성능이 높아지기 때문에 모두 선택한 것으로 판단됨
# 성능이 비슷하다면 복잡도가 낮은 모델이 좋은 모델이기에
# 6개만 선택하는 것으로 판단
# ==> AT, TAT, AH, TIT, TEY, AFDP 순으로 선택
k_f <- ols_step_forward_p(model)
k_f
plot(k_f)

# 2. backward elimination
# "No variables have been removed from the model."
# 제거할 변수가 없다고 나옴
# 이 또한 변수를 제거할수록 미미하게 계속 성능이 낮아지기 때문에 제거하지 않은 것으로 판단됨
# 성능이 비슷하다면 복잡도가 낮은 모델이 좋은 모델이므로
# forward selection의 반대로 CDP, GTEP, AP순으로 제거
k_b <- ols_step_backward_p(model)
k_b
plot(k_b)


# forward selection, backward elimination 진행 결과
# AT, TAT, AH, TIT, TEY, AFDP (여기까지 선택), AP, GTEP, CDP (여기까지 제거) 순으로
# 변수의 중요도가 높음

# 이를 설명 모델에서 각 변수의 p-value와 비교하면
summary(lm_desc)

# AT: < 2e-16
# AH: < 2e-16
# AFDP: < 2e-16
# TIT: < 2e-16
# TEY: < 2e-16
# AP: 4.52e-07
# GTEP: 0.000153
# TAT: 0.000809
# CDP: 0.004674

# TAT를 제외하고 각 변수의 p-value에 의해 유의미성이 높은 변수 순과 동일한 것을
# 확인 가능

# 선택된 변수로 (다중) 공선성을 체크해봄
lm_sel <- lm(NOX ~ AT+TAT+AH+TIT+TEY+AFDP, data = data)
vif(lm_sel)
# 여전히 TAT, TIT, TEY의 VIF가 높은 것을 확인


# 추가로 hybrid 방법인 stepwise selection을 더 진행해봄
# 3. stepwise selection (hybrid)
# AT, TAT, AH, TIT, TEY 선택 후
# TAT 제거 후
# AFDP, AP, GTEP 선택
# CDP 는 선택되지 않음
# ==> AT, AH, TIT, TEY, AFDP, AP, GTEP 를 선택
k_h <- ols_step_both_p(model)
k_h
plot(k_h)

# 선택된 변수로 다중 공선성을 체크해봄
lm_sel_h <- lm(NOX ~ AT+AH+TIT+TEY+AFDP+AP+GTEP, data = data)
vif(lm_sel_h)

# 여전히 AT, TIT, TEY, GTEP의 VIF가 높은 것을 확인

# ==> 사실상 forward selection과 backward selection 메소드에서는
# 제외할 변수가 없다고 결과를 보여주는데
# 이와 더불어 모델에서 각각의 변수의 p-value를 보면 모든 변수가 각각 매우 유의미하다
# 따라서 다중 공선성을 띄더라도 변수를 처리하기에는
# 그 변수가 각각 유의미하다고 판단하여 처리하지 않았다.


##############################################################################
# forward selection 방법과 stepwise selection 방법으로 선택된 변수 집합으로
# 예측 모델 각각 생성 및 성능 평가
# 1. forward selection (backward selection) -> 예측 모델에 적용
# (AT+TAT+AH+TIT+TEY+AFDP)

lm_pred_f <- lm(NOX ~ AT+TAT+AH+TIT+TEY+AFDP, data = tr_set)
pred_f <- predict(lm_pred_f, ts_set)
actuals_preds_f <- data.frame(cbind(actuals=ts_set$NOX, predicteds=pred_f))

# 실제 목표변수 값과 예측 값의 상관계수 ==> 0.8670373
correlation_accuracy_f <- cor(actuals_preds_f)
correlation_accuracy_f

# Min-Max Accuracy (실제값과 예측값이 얼마나 근접한가) ==> 0.9583588 (약 95.84%)
min_max_accuracy_f <- mean(apply(actuals_preds_f, 1, min) / apply(actuals_preds_f, 1, max))
min_max_accuracy_f

# MAPE (오차와 실제값의 비율)  ==> 0.04396368 (약 4.40%)
mape_f <- mean(abs((actuals_preds_f$predicteds - actuals_preds_f$actuals))/actuals_preds_f$actuals)
mape_f

# MSE (테스트 사례들에 대한 에러 자승의 평균)
# 측정결과 MSE ==> 20.64887
windows()
cvResults_f <- suppressWarnings(
  CVlm(data = tr_set, 
       form.lm=NOX ~ AT+TAT+AH+TIT+TEY+AFDP,
       m=5, 
       dots=FALSE, 
       seed=29, 
       legend.pos="topleft",  
       printit=FALSE, 
       main="Small symbols are predicted values while bigger ones are actuals.")); 

mse_f <- attr(cvResults_f, 'ms')
mse_f

# RMSE (MSE에 루트를 취한 값) rmse() 함수 말고도 sqrt 함수 (루트)를 사용해도 같음
# 측정결과 RMSE ==> 4.544102
rmse_f <- rmse(cvResults_f$NOX, cvResults_f$cvpred)
rmse_f


# 2. stepwise selection (hybrid) -> 예측 모델에 적용
# (AT+AH+TIT+TEY+AFDP+AP+GTEP)

lm_pred_h <- lm(NOX ~ AT+AH+TIT+TEY+AFDP+AP+GTEP, data = tr_set)
pred_h <- predict(lm_pred_h, ts_set)
actuals_preds_h <- data.frame(cbind(actuals=ts_set$NOX, predicteds=pred_h))

# 실제 목표변수 값과 예측 값의 상관계수 ==> 0.8677526
correlation_accuracy_h <- cor(actuals_preds_h)
correlation_accuracy_h

# Min-Max Accuracy (실제값과 예측값이 얼마나 근접한가) ==> 0.958601 (약 95.86%)
min_max_accuracy_h <- mean(apply(actuals_preds_h, 1, min) / apply(actuals_preds_h, 1, max))
min_max_accuracy_h

# MAPE (오차와 실제값의 비율)  ==> 0.04369929 (약 4.37%)
mape_h <- mean(abs((actuals_preds_h$predicteds - actuals_preds_h$actuals))/actuals_preds_h$actuals)
mape_h

# MSE (테스트 사례들에 대한 에러 자승의 평균)
# 측정결과 MSE ==> 20.58081
windows()
cvResults_h <- suppressWarnings(
  CVlm(data = tr_set, 
       form.lm=NOX ~ AT+AH+TIT+TEY+AFDP+AP+GTEP,
       m=5, 
       dots=FALSE, 
       seed=29, 
       legend.pos="topleft",  
       printit=FALSE, 
       main="Small symbols are predicted values while bigger ones are actuals.")); 

mse_h <- attr(cvResults_h, 'ms')
mse_h

# RMSE (MSE에 루트를 취한 값) rmse() 함수 말고도 sqrt 함수 (루트)를 사용해도 같음
# 측정결과 RMSE ==> 4.536608
rmse_h <- rmse(cvResults_h$NOX, cvResults_h$cvpred)
rmse_h


############################################
# 변수 선택에 따른 예측 모델 비교
# 1. full model
# 예측 변수: 9개 모두
# 실제 목표변수 값과 예측 값의 상관계수 ==> 0.8680935
correlation_accuracy
# Min-Max Accuracy ==> 0.958693
min_max_accuracy
# MAPE ==> 0.0435968
mape
# MSE ==> 10227.17
mse
# RMSE ==> 101.1295
rmse

# 2. forward selection model (원래는 full model 이나 같으나 그래프로 변수 선택함)
# 예측 변수: AT+TAT+AH+TIT+TEY+AFDP (6개) (모델 복잡도는 가장 낮을 것으로 판단)
# 실제 목표변수 값과 예측 값의 상관계수 ==> 0.8670373
correlation_accuracy_f
# Min-Max Accuracy ==> 0.9583588
min_max_accuracy_f
# MAPE ==> 0.04396368
mape_f
# MSE ==> 20.64887
mse_f
# RMSE ==> 4.544102
rmse_f

# 3. stepwise selection (hybrid) model
# 예측 변수: AT+AH+TIT+TEY+AFDP+AP+GTEP (7개)
# 실제 목표변수 값과 예측 값의 상관계수 ==> 0.8677526
correlation_accuracy_h
# Min-Max Accuracy ==> 0.958601
min_max_accuracy_h
# MAPE ==> 0.04369929
mape_h
# MSE ==> 20.58081
mse_h
# RMSE ==> 4.536608
rmse_h

# full model과 변수 선택 모델들은 MSE와 RMSE 값이 큰 차이를 보임
# 변수 선택 모델들 간의 성능 차이는 미미함
# ==> 모델의 복잡도를 고려하여 예측 변수의 수가 적은 forward selection model
# (lm_pred_f) 가 세 가지 모델 중 가장 적합하다고 판단됨.
