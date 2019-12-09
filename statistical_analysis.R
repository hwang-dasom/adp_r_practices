# Descriptive Statistics
summary(iris)
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
quantile(iris$Sepal.Length, 1/4)
quantile(iris$Sepal.Length, 3/4)
max(iris$Sepal.Length)
min(iris$Sepal.Length)

#Regression Analysis
# 모형이 통계적으로 유의? ->F통계량 / p-value <= 0.05
# 회귀계수 -> 해당 계수의 t통계량과 p-값 또는 신뢰구간
# 설명력 -> 결정계수 (0~1), the higher,  the more the model explains
# 모형이 데이터에 잘 적합하고 있나? -> 잔차 그래프 & 회귀진단
# 가정: 선형 / 독립성 / 등분산성 / 비상관성 / 정상성
# 선형: 단순회귀(설명변수 1개) & 다중회귀(설명변수 k개)
# 다항회귀 (설명변수 k개, 2차함수 이상)
# 비선형회귀

set.seed(2)
x=runif(10,0,11)
y=2+3*x+rnorm(10,0,0.2)
dfrm <- data.frame(x,y)
dfrm
lm(y~x, data=dfrm)

set.seed(2)
u <- runif(10,0,11)
v <- runif(10,11,20)
w <- runif(10,1,30)
y <- 3+0.1*u + 2 *v- 3*w + rnorm(10,0,0.1)
dfrm2 <- data.frame(y,u,v,w)
dfrm2
m <- lm(y~u+v+w)
m
summary(m)

install.packages("MASS")
library(MASS)
head(ChickWeight)
Chick <- ChickWeight[ChickWeight$Diet==1,]
head(Chick)
Chicj <- ChickWeight[ChickWeight$Chick==1,]
Chicj

lm(weight ~ Time, data=Chicj)
summary(lm(weight~Time, data=Chicj))

data(cars)
head(cars)
speed2<-cars$speed^2
cars <- cbind(speed2, cars)
head(cars)
lm(dist~speed+speed2, data=cars)
summary(lm(dist~speed+speed2, data=cars))

#example 
x <- 1:9
y<- c(5,3,2,3,4,6,10,12,18)
df1 <- data.frame(x,y)
head(df1)
plot(df1)
x2 <- x^2
df2 <- cbind(x2, df1)
### df1
lm(y~x, data=df1)
summary(lm(y~x, data=df1))
plot(lm(y~x, data=df1))
### df2
lm(y~x+x2, data=df2)
summary(lm(y~x+x2, data=df2))
plot(lm(y~x+x2, data=df2))

X1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
X2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
X3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
X4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)
Y <- c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4)

df <- data.frame(X1, X2, X3, X4, Y)
head(df)
a <- lm(Y~X1+X2+X3+X4, data=df)
a
summary(a)

b <- lm(Y~X1+X2+X4, data=df)
b
summary(b)

c <- lm(Y~X1+X2, data=df)
c
summary(c)

step(lm(Y~1, df), scope=list(lower=~1, upper=~X1+X2+X3+X4), direction="forward")
step(lm(Y~1, df), scope=list(lower=~1, upper=~X1+X2+X3+X4), direction="both")
step(lm(Y~., df), direction = "backward")

data(hills)
head(hills)
step(lm(time~1, hills), scope=list(lower=~1, upper=~dist+climb), direction="forward")

#data는 없음
step(lm(Premax~1, data=Bio), scope=list(lower=~1, upper=~age+height+weight+BMP+FEV+RV+FRC+TLC), direction="forward")
step(lm(Premax~age+height+weight+BMP+FEV+RV+FRC+TLC, data=Bio), direction="backward")
step(lm(Premax~1, data=Bio), scope=list(lower=~1, upper=~age+height+weight+BMP+FEV+RV+FRC+TLC), direction="both")

#다변량 분석
#상관분석 Correlation Analysis - 두 변수간 상관관계 왜 구함?
install.packages("Hmisc")
library(Hmisc)
data(mtcars)
head(mtcars)
drat <- mtcars$drat
disp <- mtcars$disp
plot(drat, disp)
cor(drat, disp)
rcorr(as.matrix(mtcars), type="pearson")
cov(mtcars)

rcorr(as.matrix(mtcars), type="spearman")

pairs(mtcars[1:4], pch=21)
#Metric MDS
data("eurodist")
eurodist
loc <-cmdscale(eurodist)
loc
x <- loc[,1]
y <- loc[,2]
plot(x,y, type="n", main="eurodist")
text(x,y, rownames(loc), cex=0.8)
abline(v=0, h=0)

#Non-metric MDS
data(swiss)
swiss.x <- as.matrix(swiss[, -1])
swiss.dist <- dist(swiss.x)
swiss.mds <-isoMDS(swiss.dist)
plot(swiss.mds$points, type="n")
text(swiss.mds$points, labels=as.character(1:nrow(swiss.x)))
abline(v=0, h=0, lty=2, lwd=0.5)
swiss.sh <- Shepard(swiss.dist, swiss.mds$points)
plot(swiss.sh, pch=".")
lines(swiss.sh$x, swiss.sh$yf, type = "S")

swiss.sammon <- sammon(dist(swiss.x))
plot(swiss.sammon$points, type="n")
text(swiss.sammon$points, labels=as.character(1:nrow(swiss.x)))
abline(v=0, h=0, lty=3, lwd=0.5)

# 주성분 분성 (Principal Component Analysis, PCA)
library(datasets)
data(USArrests)
summary(USArrests)
#cor=TRUE : 상관계수 행렬 => 변수별 단위 차이가 큼 -> 표준화 진행 / FALSE = 공분산행렬
pairs(USArrests, panel=panel.smooth, main="USArrests data")
fit <- princomp(USArrests, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit, type="lines")
fit$scores
biplot(fit)
round(predict(fit), 2)

#시계열 분석
# 정상성: 1. 평균 일정 2. 분산이 시점에 의존하지 않음 3. 공분산은 시차에만 의존, 시점에 의존 않음
Nile
ldeaths
plot(Nile)
plot(ldeaths)
ldeaths.decompose <- decompose(ldeaths)
ldeaths.decompose$seasonal
plot(ldeaths.decompose)
#계절성을 띄는 자료는 원 자료에서 계절요인을 빼면 됌
ldeaths.decompose.adj <- ldeaths - ldeaths.decompose$seasonal
plot(ldeaths.decompose.adj)
ldeaths
ldeaths.decompose
ldeaths.decompose.adj

#Nile강 비정상 시계열
# 1. 차분
Nile.diff1 <- diff(Nile, differences=1)
plot(Nile.diff1)
Nile.diff2 <- diff(Nile, differences=2)
plot(Nile.diff2)

acf(Nile.diff2, lag.max=20)
acf(Nile.diff2, lag.max=20, plot=FALSE)

pacf(Nile.diff2, lag.max=20)
pacf(Nile.diff2, lag.max=20, plot=F)

install.packages("forecast")
library(forecast)
# 모형 찾기
auto.arima(Nile)
# 자료를 모형에 적합
Nile.arima <- arima(Nile, order=c(1,1,1))
Nile.arima
#예측/ h => 년도
Nile.forecasts <- forecast(Nile.arima, h=10)
plot(Nile.forecasts)
