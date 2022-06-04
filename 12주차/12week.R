# install.packages('xlsx')
# install.packages('NbClust')
library(dplyr)
library(xlsx)
library(caret)
library(NbClust)
library(ggplot2)

data_receipt <- read.csv("receipt.csv",
                         fileEncoding = "euc-kr",
                         encoding = 'utf-8' )
names(data_receipt) = c("자전거 번호", "대여일시", "대여소 번호",
                        "대여소 명", "대여 거치대", "반납일시", "반납대여소 번호", "반납대여소 명",
                        "반납거치대", "이용시간", "이용거리")
data_space <- read.xlsx("space.xlsx",
                        fileEncoding = "euc-kr",
                        sheetName = "대여소현황")
names(data_space) = c("대여소 번호", "대여소 명", "소재지 위치",
                      "상세 주소", "위도", "경도", "설치 시기", "거치대수",
                      "거치대수(QR)", "운영방식")
data_receipt <- data_receipt[,-c(1:2, 4, 6, 8)]
data_space <- data_space[-c(1:4),-c(2:4, 7, 10:11)]
data_space[is.na(data_space)] <- 0
data_space$거치대수 <- as.numeric(data_space$거치대수)
data_space$'거치대수(QR)' <- as.numeric(data_space$'거치대수(QR)')

str(data_space)

for(i in 1:nrow(data_space)) {
  data_space$'거치대수'[i] <- data_space$'거치대수'[i] + data_space$'거치대수(QR)'[i]
}

data_space <- data_space[,-c(6)]

data <- merge(x=data_receipt, y=data_space, by="대여소 번호", all.x = TRUE)
data <- na.omit(data)

data$위도 <- as.numeric(data$위도)
data$경도 <- as.numeric(data$경도)

data.scaled <- scale(data[, -1])
data.kmeans <- kmeans(data.scaled, centers = 2, iter.max = 10000, nstart = 100)

set.seed(123)
data_random <- data[sample(nrow(data), 100),]

str(data_random)

data_random_scaled <- scale(t(data_random))

wssplot <- function(data, nc = 15, seed=1234) {
  wss <- (nrow(data) - 1) *sum(apply(data, 2, var))
  for(i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Numbers of Clusters",ylab="within groups sum of sqares")
}

wssplot(data_random)

data_random_scaled <- as.numeric(data_random_scaled)

nc <- NbClust(data_random_scaled, min.nc = 2, max.nc = 15, method = "kmeans")
nc2 <- NbClust(data_random_scaled, distance = "euclidean", min.nc = 2, max.nc = 15, method = "average")



par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

pairs(data_random)

new_data <- subset(data_random, select = c(거치대수, 이용시간, 이용거리))
new_data.scaled <- scale(new_data[, -1])
new_data.kmeans <- kmeans(new_data.scaled, centers = 2, iter.max = 10000, nstart = 100)

new_data$cluster <- as.factor(new_data.kmeans$cluster)

ggplot(aes(x = 이용시간, y = 이용거리), data = new_data) + geom_point(aes(color=cluster),size=2)

ggplot(aes(x = 이용시간, y = 이용거리), data = new_data) + geom_point(aes(color=거치대수),size=2)

table(new_data$거치대수, new_data$cluster)

dev.off()

data.kmeans[["betweenss"]]
data.kmeans[["withinss"]]
new_data.kmeans[["betweenss"]]
new_data.kmeans[["withinss"]]

hc <- hclust(dist(data_random), method = 'average')
hc
plot(hc, labels = data_random$거치대수)
rect.hclust(hc, k = 3)