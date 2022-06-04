library(dplyr) # seperating cols
library(tidyr) # seperating cols
library(stringr) # seperating cols
library(caret) # one hot encoding
library(arulesViz) # transaction class
library(arules)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(rattle)
library(rcompanion)
library(survival)
library(gmodels)
library(car) 

getwd()
setwd("E:\\대학\\강의\\DS\\11주차\\carAccident")

df <- read.csv("carAccident.csv",
               fileEncoding = "euc-kr",
               encoding = 'utf-8' )

# NA's - none
sum(is.na(df)) 

# seperate 발생일자 to 발생월 & 발생시간
seperated_cols3 <- data.frame(str_split_fixed(df$발생일자, "시", 2))
df$발생일자 <- seperated_cols3[, 1]

seperated_cols <- data.frame(str_split_fixed(df$발생일자, " ", 2))
seperated_cols <- rename(seperated_cols, "발생일자" = "X1", "발생시간" = "X2")
df <- data.frame(df[, names(df) != "발생일자"], seperated_cols)

seperated_cols2 <- data.frame(str_split_fixed(df$발생일자, "-", 3))
seperated_cols2 <- seperated_cols2[,2]
df <- data.frame(df[, names(df) != "발생일자"], "발생월" = seperated_cols2)

# 분석을 위한 데이터 셋
data_for_sort <- df
# 사례 수가 하나인 경우(철길건널목, 불명) 제거
data_for_sort <- data_for_sort[!(data_for_sort$도로형태_대분류 == "철길건널목" ), ]
data_for_sort <- data_for_sort[!(data_for_sort$도로형태_대분류 == "불명" ), ]
# 시군명, 사고유형, 차종 대분류, 도로형태, 위도, 경도, 법규위반내용 제거
data_for_sort<- subset(data_for_sort, select=-c(시군명, 법규위반내용, 사고유형, 도로형태, 당사자1_대분류,
                                                   당사자2_대분류, WGS84경도, WGS84위도))

# 당사자1_차종 전처리
# 경형, 소형, 중형, 승용차, 대형, 화물차 이외에는 기타 처리
table(data_for_sort$당사자1_차종)
data_for_sort$당사자1_차종 <- factor(data_for_sort$당사자1_차종,
                                levels = c("경형", "소형", "중형", "승용차", "대형", "화물차", "기타"))
data_for_sort$당사자1_차종[is.na(data_for_sort$당사자1_차종)] <- "기타"

# 당사자2_차종 전처리
table(data_for_sort$당사자2_차종)
data_for_sort$당사자2_차종 <- factor(data_for_sort$당사자2_차종,
                                levels = c("자전거", "소형", "중형", "승용차", "대형", "보행자", "없음", "기타"))
data_for_sort$당사자2_차종[is.na(data_for_sort$당사자2_차종)] <- "기타"

# 발생시간과 발생월 numeric 변환
data_for_sort$발생시간 <- as.numeric(data_for_sort$발생시간)
data_for_sort$발생월 <- as.numeric(data_for_sort$발생월)

# 주야구분명 ~ 당사자2_차종 factor 변환
data_for_sort[ , c(2:7)] = lapply(data_for_sort[ , c(2:7)], factor)

str(data_for_sort)
summary(data_for_sort)

# 크래머로 클래스 내 독립 확인
# 0~1사이 값 클수록 강한 연관성
cramerV(xtabs(~ 사고요일 + 발생년도, data = data_for_sort)) #0.03231 
cramerV(xtabs(~ 사고요일 + 주야구분명, data = data_for_sort)) #0.1002 
cramerV(xtabs(~ 사고요일 + 사고유형_대분류, data = data_for_sort)) #0.06554 
cramerV(xtabs(~ 사고요일 + 도로형태_대분류, data = data_for_sort)) #0.03739 
cramerV(xtabs(~ 사고요일 + 당사자1_차종, data = data_for_sort)) #0.04494 
cramerV(xtabs(~ 사고요일 + 당사자2_차종, data = data_for_sort)) #0.05285 

cramerV(xtabs(~ 발생년도 + 주야구분명, data = data_for_sort)) #0.02314 
cramerV(xtabs(~ 발생년도 + 사고유형_대분류, data = data_for_sort)) #0.04434  
cramerV(xtabs(~ 발생년도 + 도로형태_대분류, data = data_for_sort)) #0.07689  
cramerV(xtabs(~ 발생년도 + 당사자1_차종, data = data_for_sort)) #0.3962  
cramerV(xtabs(~ 발생년도 + 당사자2_차종, data = data_for_sort)) #0.2152 

cramerV(xtabs(~ 주야구분명 + 사고유형_대분류, data = data_for_sort)) #0.1588  
cramerV(xtabs(~ 주야구분명 + 도로형태_대분류, data = data_for_sort)) #0.0388  
cramerV(xtabs(~ 주야구분명 + 당사자1_차종, data = data_for_sort)) #0.1792  
cramerV(xtabs(~ 주야구분명 + 당사자2_차종, data = data_for_sort)) #0.1727 

cramerV(xtabs(~ 사고유형_대분류 + 도로형태_대분류, data = data_for_sort)) #0.5855 
cramerV(xtabs(~ 사고유형_대분류 + 당사자1_차종, data = data_for_sort)) #0.1058  
cramerV(xtabs(~ 사고유형_대분류 + 당사자2_차종, data = data_for_sort)) #0.816

cramerV(xtabs(~ 도로형태_대분류 + 당사자1_차종, data = data_for_sort)) #0.07364  
cramerV(xtabs(~ 도로형태_대분류 + 당사자2_차종, data = data_for_sort)) #0.08842 

cramerV(xtabs(~ 당사자1_차종 + 당사자2_차종, data = data_for_sort)) #0.1794 

#####################################################################################################
#####################################################################################################

# CrossTable 을 이용해 카이제곱과 p-value 구함
# 일반적인 속성간의 독립
# 0.05 보다 크면 독립 / 0.05 보다 작으면 종속

# 사망자수 - 다른 변수 간의 독립성 확인
CrossTable(data_for_sort$사망자수, data_for_sort$발생년도, chisq = T) # 0.2528718 -> 독립
CrossTable(data_for_sort$사망자수, data_for_sort$주야구분명, chisq = T) # 0.9030096 -> 독립
CrossTable(data_for_sort$사망자수, data_for_sort$발생월, chisq = T) # 0.05622273 -> 독립
CrossTable(data_for_sort$사망자수, data_for_sort$발생시간, chisq = T) # 0.4671762 -> 독립

CrossTable(data_for_sort$사망자수, data_for_sort$사고요일, chisq = T) # 0.0409237
CrossTable(data_for_sort$사망자수, data_for_sort$사고유형_대분류, chisq = T) # 5.694131e-16
CrossTable(data_for_sort$사망자수, data_for_sort$도로형태_대분류, chisq = T) # 0.001907891
CrossTable(data_for_sort$사망자수, data_for_sort$당사자1_차종, chisq = T) # 0.02024232
CrossTable(data_for_sort$사망자수, data_for_sort$당사자2_차종, chisq = T) # 1.492005e-12
CrossTable(data_for_sort$사망자수, data_for_sort$중상자수, chisq = T) # 2.351003e-69
CrossTable(data_for_sort$사망자수, data_for_sort$부상자수, chisq = T) # 6.019592e-300
CrossTable(data_for_sort$사망자수, data_for_sort$부상신고자수, chisq = T) # 0.00406051
CrossTable(data_for_sort$사망자수, data_for_sort$경상자수, chisq = T) # 9.779603e-43

# 중상자수 - 다른 변수 간의 독립성 확인
CrossTable(data_for_sort$중상자수, data_for_sort$발생년도, chisq = T) # 0.7238838 -> 독립
CrossTable(data_for_sort$중상자수, data_for_sort$발생월, chisq = T) # 0.09711521 -> 독립
CrossTable(data_for_sort$중상자수, data_for_sort$주야구분명, chisq = T) # 0.1069197 -> 독립
CrossTable(data_for_sort$중상자수, data_for_sort$사고요일, chisq = T) # 0.6039386 -> 독립
CrossTable(data_for_sort$중상자수, data_for_sort$도로형태_대분류, chisq = T) # 0.9987172 -> 독립

CrossTable(data_for_sort$중상자수, data_for_sort$경상자수, chisq = T) # 0
CrossTable(data_for_sort$중상자수, data_for_sort$부상자수, chisq = T) # 0
CrossTable(data_for_sort$중상자수, data_for_sort$부상신고자수, chisq = T) # 0
CrossTable(data_for_sort$중상자수, data_for_sort$발생시간, chisq = T) # 0.000367956
CrossTable(data_for_sort$중상자수, data_for_sort$사고유형_대분류, chisq = T) # 1.506517e-84
CrossTable(data_for_sort$중상자수, data_for_sort$당사자1_차종, chisq = T) # 4.105797e-05
CrossTable(data_for_sort$중상자수, data_for_sort$당사자2_차종, chisq = T) # 2.604696e-96

# 경상자수
CrossTable(data_for_sort$경상자수, data_for_sort$발생년도, chisq = T) # 0.732966 -> 독립
CrossTable(data_for_sort$경상자수, data_for_sort$발생월, chisq = T) # 0.6612517 -> 독립
CrossTable(data_for_sort$경상자수, data_for_sort$사고요일, chisq = T) # 0.2002055 -> 독립
CrossTable(data_for_sort$경상자수, data_for_sort$도로형태_대분류, chisq = T) # 1 -> 독립
CrossTable(data_for_sort$경상자수, data_for_sort$발생시간, chisq = T) # 0.1781023 -> 독립

CrossTable(data_for_sort$경상자수, data_for_sort$주야구분명, chisq = T) # 0.0002013099
CrossTable(data_for_sort$경상자수, data_for_sort$부상자수, chisq = T) # 0
CrossTable(data_for_sort$경상자수, data_for_sort$부상신고자수, chisq = T) # 0
CrossTable(data_for_sort$경상자수, data_for_sort$사고유형_대분류, chisq = T) # 1.007466e-92
CrossTable(data_for_sort$경상자수, data_for_sort$당사자1_차종, chisq = T) # 0.001007078
CrossTable(data_for_sort$경상자수, data_for_sort$당사자2_차종, chisq = T) # 7.341964e-120

# 부상자수
CrossTable(data_for_sort$부상자수, data_for_sort$발생월, chisq = T) # 0.05297024 -> 독립
CrossTable(data_for_sort$부상자수, data_for_sort$사고요일, chisq = T) # 0.5438399 -> 독립
CrossTable(data_for_sort$부상자수, data_for_sort$도로형태_대분류, chisq = T) # 1 -> 독립
CrossTable(data_for_sort$부상자수, data_for_sort$발생시간, chisq = T) # 0.08959627 -> 독립
CrossTable(data_for_sort$부상자수, data_for_sort$발생년도, chisq = T) # 0.1934117 -> 독립

CrossTable(data_for_sort$부상자수, data_for_sort$주야구분명, chisq = T) # 0.001880091
CrossTable(data_for_sort$부상자수, data_for_sort$부상신고자수, chisq = T) # 0
CrossTable(data_for_sort$부상자수, data_for_sort$사고유형_대분류, chisq = T) # 1.536557e-169
CrossTable(data_for_sort$부상자수, data_for_sort$당사자1_차종, chisq = T) # 0.0001114344
CrossTable(data_for_sort$부상자수, data_for_sort$당사자2_차종, chisq = T) # 2.538707e-213

# 부상신고자수
CrossTable(data_for_sort$부상신고자수, data_for_sort$발생월, chisq = T) # 0.2297731 -> 독립
CrossTable(data_for_sort$부상신고자수, data_for_sort$사고요일, chisq = T) # 0.7372429 -> 독립
CrossTable(data_for_sort$부상신고자수, data_for_sort$도로형태_대분류, chisq = T) # 0.999934 -> 독립
CrossTable(data_for_sort$부상신고자수, data_for_sort$발생시간, chisq = T) # 0.5880077 -> 독립
CrossTable(data_for_sort$부상신고자수, data_for_sort$발생년도, chisq = T) # 0.3799437 -> 독립
CrossTable(data_for_sort$부상신고자수, data_for_sort$주야구분명, chisq = T) # 0.5062259 -> 독립
CrossTable(data_for_sort$부상신고자수, data_for_sort$당사자1_차종, chisq = T) # 0.09139285 -> 독립

CrossTable(data_for_sort$부상신고자수, data_for_sort$사고유형_대분류, chisq = T) # 3.252028e-05
CrossTable(data_for_sort$부상신고자수, data_for_sort$당사자2_차종, chisq = T) # 1.823597e-13

# 주야구분명
CrossTable(data_for_sort$주야구분명, data_for_sort$발생월, chisq = T) # 0.5598864 -> 독립
CrossTable(data_for_sort$주야구분명, data_for_sort$도로형태_대분류, chisq = T) # 0.1709193 -> 독립
CrossTable(data_for_sort$주야구분명, data_for_sort$발생년도, chisq = T) # 0.7376154 -> 독립

CrossTable(data_for_sort$주야구분명, data_for_sort$당사자1_차종, chisq = T) # 4.578011e-33
CrossTable(data_for_sort$주야구분명, data_for_sort$발생시간, chisq = T) # 0
CrossTable(data_for_sort$주야구분명, data_for_sort$사고요일, chisq = T) # 2.234537e-09
CrossTable(data_for_sort$주야구분명, data_for_sort$사고유형_대분류, chisq = T) # 6.198834e-28
CrossTable(data_for_sort$주야구분명, data_for_sort$당사자2_차종, chisq = T) # 7.509233e-30

# 사고유형_대분류
CrossTable(data_for_sort$사고유형_대분류, data_for_sort$발생월, chisq = T) # 2.234191e-06
CrossTable(data_for_sort$사고유형_대분류, data_for_sort$도로형태_대분류, chisq = T) # 0
CrossTable(data_for_sort$사고유형_대분류, data_for_sort$발생년도, chisq = T) # 0.01076083
CrossTable(data_for_sort$사고유형_대분류, data_for_sort$당사자1_차종, chisq = T) # 2.714716e-27
CrossTable(data_for_sort$사고유형_대분류, data_for_sort$발생시간, chisq = T) # 7.692225e-44
CrossTable(data_for_sort$사고유형_대분류, data_for_sort$사고요일, chisq = T) # 1.892292e-07
CrossTable(data_for_sort$사고유형_대분류, data_for_sort$당사자2_차종, chisq = T) # 0

# 도로형태_대분류
CrossTable(data_for_sort$도로형태_대분류, data_for_sort$발생월, chisq = T) # 0.5005402 -> 독립
CrossTable(data_for_sort$도로형태_대분류, data_for_sort$사고요일, chisq = T) # 0.2092921 -> 독립

CrossTable(data_for_sort$도로형태_대분류, data_for_sort$발생년도, chisq = T) # 3.533365e-20
CrossTable(data_for_sort$도로형태_대분류, data_for_sort$당사자1_차종, chisq = T) # 4.759628e-16
CrossTable(data_for_sort$도로형태_대분류, data_for_sort$발생시간, chisq = T) # 0.02023533
CrossTable(data_for_sort$도로형태_대분류, data_for_sort$당사자2_차종, chisq = T) # 3.271172e-25

# 발생시간
CrossTable(data_for_sort$발생시간, data_for_sort$발생년도, chisq = T) # 0.4293649 -> 독립

CrossTable(data_for_sort$발생시간, data_for_sort$발생월, chisq = T) # 8.03415e-07
CrossTable(data_for_sort$발생시간, data_for_sort$사고요일, chisq = T) # 1.106866e-05
CrossTable(data_for_sort$발생시간, data_for_sort$당사자1_차종, chisq = T) # 1.902484e-23
CrossTable(data_for_sort$발생시간, data_for_sort$당사자2_차종, chisq = T) # 1.264588e-37

# 발생월
CrossTable(data_for_sort$발생월, data_for_sort$사고요일, chisq = T) # 0.1047856 -> 독립

CrossTable(data_for_sort$발생월, data_for_sort$발생년도, chisq = T) # 0.001188108
CrossTable(data_for_sort$발생월, data_for_sort$당사자1_차종, chisq = T) # 7.936617e-05
CrossTable(data_for_sort$발생월, data_for_sort$당사자2_차종, chisq = T) # 0.001664104

# 사고요일
CrossTable(data_for_sort$사고요일, data_for_sort$발생년도, chisq = T) # 0.6314889 -> 독립

CrossTable(data_for_sort$사고요일, data_for_sort$당사자1_차종, chisq = T) # 0.004177008
CrossTable(data_for_sort$사고요일, data_for_sort$당사자2_차종, chisq = T) # 6.93223e-05

# 발생년도
CrossTable(data_for_sort$발생년도, data_for_sort$당사자1_차종, chisq = T) # 0
CrossTable(data_for_sort$발생년도, data_for_sort$당사자2_차종, chisq = T) # 1.513828e-227

#당사자1_차종
CrossTable(data_for_sort$당사자1_차종, data_for_sort$당사자2_차종, chisq = T) # 6.319326e-181

# 사망자수와 종속 관계
# - 사고요일,사고유형_대분류,도로형태_대분류,당사자1_차종,당사자2_차종,중상자수,부상자수,부상신고자수,경상자수

# 중상자수와 종속 관계
# - 경상자수,부상자수,부상신고자수,발생시간,사고유형_대분류,당사자1_차종,당사자2_차종

# 경상자수와 종속 관계
# - 주야구분명,부상자수,부상신고자수,사고유형_대분류,당사자1_차종,당사자2_차종

# 부상자수와 종속 관계
# - 주야구분명,부상신고자수,사고유형_대분류,당사자1_차종,당사자2_차종

# 부상신고자수와 종속 관계
# - 사고유형_대분류,당사자2_차종

# 주야구분명와 종속 관계
# - 발생시간,사고요일,사고유형_대분류,당사자1_차종,당사자2_차종

# 사고유형_대분류와 종속 관계
# 발생시간,사고요일,발생월,도로형태_대분류,발생년도,당사자1_차종,당사자2_차종

# 도로형태_대분류와 종속 관계
# 발생년도,발생시간,당사자1_차종,당사자2_차종

# 발생시간과 종속 관계
# 발생월,사고요일,당사자1_차종,당사자2_차종

# 발생월과 종속 관계
# 발생년도,당사자1_차종,당사자2_차종

# 사고요일과 종속 관계
# 당사자1_차종,당사자2_차종

# 발생년도 종속 관계
# 당사자1_차종,당사자2_차종

# 당사자1_차종, 당사자2_차종 서로 종속 관계

#####################################################################################################

# 회귀 분석

# 사망자수와의 회귀 계수 분석
lm_desc <- lm(사망자수~., data=data_for_sort)
lm_desc
# 양의 회귀 계수는 비례 관계
# 음의 회귀 계수는 반비례 관계

# p-value값 분석
# 사망자수 - 다른 변수 간 p-value
# 0.05 보다 크면 독립 / 0.05 보다 작으면 종속
summary(lm_desc)

#####################################################################################################


#연관성 분석 위한 전처리

# 중상자수, 부상자수, 경상자수, 부상신고자수 전처리 
# 위 네 개 변수를 사상자수 변수로 통합
# 20 명 이상인 경우 대형사고, 나머지 일반교통사고로 분류
temp.df <- subset(data_for_sort, select=c(중상자수, 부상자수, 경상자수, 부상신고자수))
temp.df$사상자수 <- apply(temp.df, 1, sum)

data_for_sort <- subset(data_for_sort, select=-c(중상자수, 부상자수, 경상자수, 부상신고자수))
data_for_sort <- data.frame(data_for_sort, "사상자수" = temp.df$사상자수)

table(data_for_sort$사상자수)

data_for_sort$사상자수 <- ifelse(data_for_sort$사상자수 >= 20, "대형사고", "일반교통사고")
data_for_sort$사상자수 <- as.factor(data_for_sort$사상자수)

# 사망자가 3명 이상인 경우 대형사고, 나머지 일반교통사고로 분류
data_for_sort$사망자수 <- ifelse(data_for_sort$사망자수 >= 3, "대형사고", "일반교통사고")
data_for_sort$사망자수 <- as.factor(data_for_sort$사망자수)

# 0~5시는 새벽, 5~9시는 아침, 9~17시는 낮, 17~21시는 저녁, 21~24시는 밤
data_for_sort$발생시간[data_for_sort$발생시간 >= 0 & data_for_sort$발생시간 < 5] = "새벽"
data_for_sort$발생시간[data_for_sort$발생시간 >= 5 & data_for_sort$발생시간 < 9] = "아침"
data_for_sort$발생시간[data_for_sort$발생시간 >= 9 & data_for_sort$발생시간 < 17] = "낮"
data_for_sort$발생시간[data_for_sort$발생시간 >= 17 & data_for_sort$발생시간 < 21] = "저녁"
data_for_sort$발생시간[data_for_sort$발생시간 >= 21& data_for_sort$발생시간 < 24] = "밤"
data_for_sort$발생시간[data_for_sort$발생시간 != "새벽" &
                     data_for_sort$발생시간 != "아침" &
                     data_for_sort$발생시간 != "저녁" &
                     data_for_sort$발생시간 != "밤"] = "낮"

data_for_sort$발생시간 <- as.factor(data_for_sort$발생시간)
data_for_sort$발생년도 <- as.factor(data_for_sort$발생년도)
data_for_sort$발생월 <- as.factor(data_for_sort$발생월)

# transaction class로 변환
df.trans <- as(data_for_sort, "transactions")

summary(df.trans)
inspect(head(df.trans))
df.trans@data
df.trans@itemInfo

frequentItems <- eclat (df.trans, parameter = list(supp = 0.07, maxlen = 15))
inspect(head(sort(frequentItems, by="support", decreasing = TRUE)))
frequentItems@items@itemInfo

# 전체 변수를 사용한 연관성분석
# support는 
# lift 가 1에 가깝다면 독립(흥미롭지 않음), 1이상인 경우 양의 상관관계, 1이하인 경우 음의 상관관계
rules <- apriori (data_for_sort, parameter = list(supp = 0.01, conf = 0.5))

# inspect(head(sort(rules, by="support", decreasing = TRUE), 3))
inspect(head(sort(rules, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(rules, by="lift", decreasing = TRUE), 3))
inspect(head(sort(rules, by="coverage", decreasing = TRUE), 3))

# 사망자수를 rhs로 지정하여 연관성 분석
rules_rhs.사망자수 <- subset(rules, subset = rhs %pin% "사망자수")

inspect(head(sort(rules_rhs.사망자수, by="support", decreasing = TRUE), 3))
inspect(head(sort(rules_rhs.사망자수, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(rules_rhs.사망자수, by="lift", decreasing = TRUE), 3))

# 사상자수를 rhs로 지정하여 연관성 분석
rules_rhs.사상자수 <- subset(rules, subset = rhs %pin% "사상자수")

inspect(head(sort(rules_rhs.사망자수, by="support", decreasing = TRUE), 3))
inspect(head(sort(rules_rhs.사망자수, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(rules_rhs.사망자수, by="lift", decreasing = TRUE), 3))


# rhs : 사망자수
# lhs : 사고요일,사고유형_대분류,도로형태_대분류,당사자1_차종,당사자2_차종,사상자
df1 <- subset(data_for_sort, select = c("사고요일", "사고유형_대분류", "도로형태_대분류", 
                                        "당사자1_차종", "당사자2_차종", "사상자수", "사망자수"))
temp_rule <- apriori (df1, parameter = list(supp = 0.01, conf = 0.5))
subset_rules1 <- subset(temp_rule, subset = rhs %pin% "사망자수")

inspect(head(sort(subset_rules1, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules1, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules1, by="lift", decreasing = TRUE), 3))


# rhs : 사상자
# lhs : 발생시간,사고유형_대분류,당사자1_차종,당사자2_차종
df2 <- subset(data_for_sort, select = c("사상자수", "발생시간", "사고유형_대분류", "당사자1_차종", "당사자2_차종"))
temp_rule <- apriori (df2, parameter = list(supp = 0.01, conf = 0.5))
subset_rules2 <- subset(temp_rule, subset = rhs %pin% "사상자수")

inspect(head(sort(subset_rules2, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules2, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules2, by="lift", decreasing = TRUE), 3))


# rhs : 주야구분명
# lhs : 발생시간,사고요일,사고유형_대분류,당사자1_차종,당사자2_차종
df3 <- subset(data_for_sort, select = c("주야구분명","발생시간","사고요일","사고유형_대분류","당사자1_차종","당사자2_차종"))
temp_rule <- apriori (df3, parameter = list(supp = 0.01, conf = 0.5))
subset_rules3 <- subset(temp_rule, subset = rhs %pin% "주야구분명")

inspect(head(sort(subset_rules3, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules3, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules3, by="lift", decreasing = TRUE), 3))

# rhs : 사고유형_대분류
# lhs : 발생시간,사고요일,발생월,도로형태_대분류,발생년도,당사자1_차종,당사자2_차종
df4 <- subset(data_for_sort, select = c("사고유형_대분류","발생시간","사고요일","발생월",
                                        "도로형태_대분류","발생년도","당사자1_차종","당사자2_차종"))
temp_rule <- apriori (df4, parameter = list(supp = 0.01, conf = 0.5))
subset_rules4 <- subset(temp_rule, subset = rhs %pin% "사고유형_대분류")

inspect(head(sort(subset_rules4, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules4, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules4, by="lift", decreasing = TRUE), 3))

# rhs : 도로형태_대분류
# lhs : 발생년도,발생시간,당사자1_차종,당사자2_차종
df5 <- subset(data_for_sort, select = c("도로형태_대분류","발생년도","발생시간","당사자1_차종","당사자2_차종"))
temp_rule <- apriori (df5, parameter = list(supp = 0.01, conf = 0.5))
subset_rules5 <- subset(temp_rule, subset = rhs %pin% "도로형태_대분류")

inspect(head(sort(subset_rules5, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules5, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules5, by="lift", decreasing = TRUE), 3))

# rhs : 발생시간
# lhs : 발생월,사고요일,당사자1_차종,당사자2_차종
## support 0.01, confidence 0.5를 넘는 집합이 1개
df6 <- subset(data_for_sort, select = c("발생시간","발생월","사고요일","당사자1_차종","당사자2_차종"))
temp_rule <- apriori (df6, parameter = list(supp = 0.01, conf = 0.5))
subset_rules6 <- subset(temp_rule, subset = rhs %pin% "발생시간")

inspect(head(sort(subset_rules6, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules6, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules6, by="lift", decreasing = TRUE), 3))

inspect(subset_rules6)

# rhs : 발생월
# lhs : 발생년도,당사자1_차종,당사자2_차종
## support 0.01, confidence 0.5를 넘는 집합이 없음
df7 <- subset(data_for_sort, select = c("발생월","발생년도","당사자1_차종","당사자2_차종"))
temp_rule <- apriori (df7, parameter = list(supp = 0.01, conf = 0.5))
subset_rules7 <- subset(temp_rule, subset = rhs %pin% "발생월")

inspect(head(sort(subset_rules7, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules7, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules7, by="lift", decreasing = TRUE), 3))

# rhs : 사고요일과 종속 관계
# lhs : 당사자1_차종,당사자2_차종
## support 0.01, confidence 0.5를 넘는 집합이 없음
df8 <- subset(data_for_sort, select = c("사고요일","당사자1_차종","당사자2_차종"))
temp_rule <- apriori (df8, parameter = list(supp = 0.01, conf = 0.5))
subset_rules8 <- subset(temp_rule, subset = rhs %pin% "사고요일")

inspect(head(sort(subset_rules8, by="support", decreasing = TRUE)))
inspect(head(sort(subset_rules8, by="confidence", decreasing = TRUE)))
inspect(head(sort(subset_rules8, by="lift", decreasing = TRUE)))

# rhs : 발생년도
# lhs : 당사자1_차종,당사자2_차종
## support 0.01, confidence 0.5를 넘는 집합이 없음
df9 <- subset(data_for_sort, select = c("발생년도","당사자1_차종","당사자2_차종"))
temp_rule <- apriori (df9, parameter = list(supp = 0.01, conf = 0.5))
subset_rules9 <- subset(temp_rule, subset = rhs %pin% "발생년도")

inspect(head(sort(subset_rules9, by="support", decreasing = TRUE), 3))
inspect(head(sort(subset_rules9, by="confidence", decreasing = TRUE), 3))
inspect(head(sort(subset_rules9, by="lift", decreasing = TRUE), 3))

# rhs : 당사자2_차종
# lhs : 당사자1_차종
## support 0.01, confidence 0.5를 넘는 집합이 없음
df10 <- subset(data_for_sort, select = c("당사자1_차종", "당사자2_차종"))
temp_rule <- apriori (df10, parameter = list(supp = 0.01, conf = 0.5))
subset_rules10 <- subset(temp_rule, subset = rhs %pin% "당사자2_차종")

inspect(head(sort(subset_rules10, by="support", decreasing = TRUE)))
inspect(head(sort(subset_rules10, by="confidence", decreasing = TRUE)))
inspect(head(sort(subset_rules10, by="lift", decreasing = TRUE)))

