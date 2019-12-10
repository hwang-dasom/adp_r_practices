# R을 활용한 빅데이터 분석
# Chapter2. R 프로그래밍 기초

# matrix
matrix(1:20, ncol=5)
array(1:20, dim=c(4,5))
array(1:20, dim=c(4,4,3))

a <- 1:9
dim(a)
a
dim(a) <- c(3,3)
a
dim(a) <-c(2,3)

b <- list(1:9)
b
dim(b)
dim(b) <-c(3,3)

c <- list(1,2,3,4,5,6,7,8,9)
c
dim(c)
dim(c) <- c(3,3)

d <- list(c(1:3), c(2:5))
d
d[1]
d[[1]]

lst <- list(c(1,2), c(3,4), c(5,6), c(7,8))
lst
lst[c(1,4)]
lst[1,4]
lst[[1,4]]
lst[[1],[4]]
lst[[1]] <-"moe"
lst[[1]]
names(lst) <-c("first", "2nd", "3rd", "4th")
lst
lst[1]
lst[[1]]
year <- list(1975, 1978, 1980, 2010, 2019)
year
class(year[[3]])
class(year[3])
cat(year[[3]])

iq.square <- list(c(110, 130, 145, 160))
mean(iq.square)
mean(unlist(iq.square))
cat("IQ score: ", unlist(iq.square), "\n")

lst <- list(NULL, 1,2,3,4,5)
lst
sapply(lst, is.null)
lst[sapply(lst, is.null)] <-NULL
lst

capital <- c("seoul", "rome", "vienna", "bern")
values <- 1:4
nal <- data.frame(capital, values)
nal
names(nal)
names(nal) <- c("city", "rank")
names(nal)

vec <- c("100", "95", "90", "85")
nal$ncol <- vec
nal
nal <- subset(nal, select=-ncol)
nal
rownames(nal)
colnames(nal)

lapply(as.list(iris), summary)
summary(iris)

# 범주형 데이터 - 독립성 검정 : 카이제곱 검정
iris$Species2 <- as.factor(sample(1:5, 150, replace=TRUE))
str(iris)
summary(table(iris$Species, iris$Species2))
#Number of cases in table: 150 
#Number of factors: 2 
#Test for independence of all factors:
#  Chisq = 5.851, df = 8, p-value = 0.664
# p-value 0.664 > 0.05 : 해당 변수 서로 독립적

#정규화(z점수): z 점수 구할 떄, scale 함수 사용- 벡터, 행렬, 데이터 프레임
scale(iris$Sepal.Length)
# x 값에 대한 정규화: ( X - mean) / sd
(3.8 - mean(iris$Sepal.Length)) / sd(iris$Sepal.Length)

# t-검정(모평균 검정)
# 표본을 이용하여 모집단의 평균을 알고 싶을 때, t.test
x <- rnorm(40, mean=95, sd=10)
t.test(x, mu=90) # 표본 x 에 대하여 모평균이 90 인지 확인
# p-value 0.001454 < 0.05 : 기각
t.test(x, mu=96)
# p-value 0.9275 > 0.05 : 채택

# 신뢰구간
# 표본을 이용하여 모집단의 신뢰구간을 구하고 싶으면 표본에 x 적용
# 99% 신뢰구수준의 구간을 구하고 싶으면 conf.level 인자사용
x <- rnorm(40, mean=95, sd=10)
t.test(x)
t.test(x, conf.level=0.99)

#모비율 검정
# 표본비율을 이용하여 모집단 비율 구하기
prop.test(56, 100, 0.5) 
# 현재 승률 0.56, 앞으로 경기 절반 이상을 이길 수 있나 / 단, 귀무가설 P <=0.5
# 승률이 0.5 초과이라는 대립가설에 대한 검정
# p-value 0.27 > 0.05 : 귀무가설 기각 안됌 - 채택
prop.test(56, 100, 0.5, alternative = "greater")

#정규서 검정 - 히스토그램 또는 Q-Q plot 함께 사용
attach(iris)
install.packages("nortest")
library(nortest)
shapiro.test(Sepal.Length) # p-value > 0.05 : 정규분포, otherwise, !정규분포
par(mfrow=c(1,2))
hist(Sepal.Length)
qqnorm(Sepal.Length)
qqline(Sepal.Length, col="red", lwd=2)

#유의성 검정
# 변수사이의 상관관계가 통계적으로 유의한지 확인: cor.test() - 검정과 신뢰구간 계산
# default: 피어슨(정규분포) / 스피어만(!정규분포)
# 귀무가설: 상관관계 없다 / 대립가설(우리가 확인하고자 하는 내용) : 상관관계 있다
cor.test(iris$Sepal.Length, iris$Sepal.Width)
# p-value 0.1519 > 0.05 : 귀무가설 채택
