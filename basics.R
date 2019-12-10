#Matrix example
mx = matrix(c(1,2,3,4,5,6), ncol=2)
matrix(c(1,2,3,4,5,6), nrow=2)
matrix(c(1,2,3,4,5,6), ncol=2, byrow=T)

# rbind(), cbind() example
r1 = c(10,10)
c1 = c(20,20,20)

rbind(mx, r1)
m3 <- cbind(mx, c1)

m1 <- rbind(r1, mx )
m2<- cbind(c1, mx)

cbind(m2, m3)

#data.frame() example
income = c(100,200,150, 300, 900)
car = c("kia","SMQ", "BTS", "JYP", "HD")
marriage= c(F,F,F,T,T)
mydat = data.frame(income, car, marriage)
mydat

# Set working directory for data
getwd()
setwd()
setwd("/Users/somiee/Desktop/data/dds_datasets")

# read data1. read.table()
data1<-read.table("dds_ch2_nyt/nyt1.csv", header=T, sep=",")
head(data1)

#read.csv()
data2 <-read.csv("dds_ch2_nyt/nyt1.csv", header=T)
head(data2)

#EXCEL (Suppose RODBC has installed)
library(RODBC)
#확장자 생략
new <- odbcConnectExcel("dds_ch2_rollingsales/rollingsales_bronx")
yourdata <-sqlfetch(new, "Sheet1")
close(new)

#Basic functions
rep(1,3)
seq(1,3)
1:3
seq(1, 11, by=2) #등차수열
seq(1, 11, length=5)
rep(2:5, 3)
rep(2:5, times=3)
rep(2:5, each=3)

#vector operations
a<-1:10
a+a
a-a
a*a
a/a

#벡터는 기본적으로 열벡터
a = c(1,2,3)
a
t(a) # Transpose - 전치행렬
A = a%*%t(a) # 행렬곱
A

mx = matrix(c(21,43,12,43,64,23,1,23,93), nrow=3)
mx
5*mx # 스칼라 곱
solve(mx) # 역행렬

# Basic Statistics
a <- 1:10
a
mean(a)
var(a)
sd(a)
sum(a)
median(a)
log(a)
b=log(a)
cov(a,b) # covariance
cor(a,b) #correlation
summary(a) # 각 사분위수, 최소, 최대, 중앙, 평균

#Data handling
b = c("a", "b", "c", "d", "e")
b

b[2]
b[-4]
b[c(2,3)]
b[2:3]
b[1:3]
b[c(1,3)]

mydat[3,2]
mydat
mydat[,3]
mydat[,2]
mydat[4,]

# Iteration and conditions
# FOR loop
a = c()
for(i in 1:9){
  a[i] = i*i
}
a

isum= 0
for(i in 1:100){
  isum = isum+i
}
cat("Summation from 1 to 100 = ", isum, "\n")

# WHILE
x=1
while(x<5){
  x = x+1
  print(x)
}

#IF ~ ELSE
StatScore <- rep(68:97)
length(StatScore)
over78 <- rep(0,30)
over78
for(i in 1:30){
  if(StatScore[i] >= 78) over78[i] <- 1
  else over78[i] <- 0
}
over78
sum(over78)

addto = function(a){
  isum=0
  for(i in 1:a){
    isum= isum+i
  }
  print(isum)
}
addto(100)
addto(50)

#Etc.
# 1. paste()
number <- 1:10
alphabet <- c("a", "b", "c")
paste(number, alphabet)
paste(number, alphabet, sep=" to the ")
# 2. substr
substr("BigDataAnalysis", 1, 4)
country <- c("Korea", "China", "Singapore", "Russia")
substr(country, 1,3)

#3. as.data.frame / as.list / as.matrix / as.vector / as.factor
as.integer(3.14)
as.numeric("foo")
as.character(101)
as.numeric(FALSE)
as.logical(0.45)
as.matrix(mydat)
mydat

# Date
Sys.Date()
#str -> Date
as.Date("2015-01-13")
as.Date("01/13/2015")
as.Date("01/13/2015", format="%m/%d/%Y")
# Date ->str
format(Sys.Date())
as.character(Sys.Date())
format(Sys.Date(), format="%m/%d/%Y")

format(Sys.Date(), '%a')
format(Sys.Date(), '%b')
format(Sys.Date(), '%m')
format(Sys.Date(), '%d')       
format(Sys.Date(), '%y')
format(Sys.Date(), '%Y')

#Graph
height <- c(170, 168, 174, 175, 188, 165, 165, 190, 173, 168, 159, 170, 184, 155, 165)
weight <- c(68, 65, 74, 77, 92, 63, 67, 95, 72, 69, 60, 69, 73, 56, 55)
#Scatter graph
plot(height, weight)
plot(weight ~ height)

library(datasets)
iris
# 산점도 행렬
pairs(iris[1:4], main="Anderson's Iris Data -- 3 species", 
      pch=21 , bg = c("red", "green3", "blue")[unclass(iris$Species)])

# Histogram
StatScore <- c(88, 90, 78, 84, 76, 68, 50, 48, 33, 70, 48, 66, 88, 
               96, 79, 65, 27, 88, 96, 33, 64, 48, 77, 18, 26, 44,
               48, 68, 77, 64, 88, 95, 79, 88, 49, 30, 29, 10, 49, 88)
hist(StatScore, prob=T)
hist(StatScore, prob=F)
boxplot(StatScore)

# RESHAPE
install.packages("reshape")
library(reshape)
data("airquality")
head(airquality)
head(airquality, 10)
names(airquality)
names(airquality) <- tolower(names(airquality))
names(airquality)
#melt()
aqm <- melt(airquality, id=c("month", "day"), na.rm=TRUE)
aqm
#cast()
a <- cast(aqm, day ~ month ~ variable)
a
b <- cast(aqm, month~variable, mean)
b
c <- cast(aqm, month ~. | variable, mean)
c
d <- cast(aqm, month ~ variable, mean, margins = c("grand_row", "grand_col"))
d
e <- cast(aqm, day ~ month, mean, subset=variable=="ozone")
e
#_X1 : min / _X2: MAX
f <- cast(aqm, month ~ variable, range)
f

#sqldf
install.packages("sqldf")
library(sqldf)
data(iris)
sqldf("select * from iris")
sqldf("select * from iris limit 10")
head(iris, 10)
sqldf("select count(*) from iris where Species like 'se%'")

#plyr / d = Data Frame, a = Array, l = List
install.packages("plyr")
library(plyr)
set.seed(1)
# runif(# of rand.number, min, MAX)
d = data.frame(year = rep(2012:2014, each=6), count=round(runif(9,0,20)))
print(d)
ddply(d, "year", function(x){
  mean.count = mean(x$count)
  sd.count = sd(x$count)
  cv = sd.count/mean.count
  data.frame(cv.count=cv)
})

ddply(d, "year", summarise, mean.count = mean(count))
ddply(d, "year", summarise, total.count=sum(count))
ddply(d, "year", transform, total.count = sum(count))

# Data table
install.packages("data.table")
library(data.table)

DT= data.table(x=c("b", "b", "b", "a", "a"), v=rnorm(5))
DT
data(cars)
head(cars)
CARS <- data.table(cars)
head(CARS)
tables()
sapply(CARS, class)
DT
DT[2,]
DT[2,1]
DT[DT$x=="b",]
setkey(DT, x)
DT
tables()
DT["b",]
DT["b", mult="first"]
DT["b", mult="last"]

# DataFrame vs. DataTable
grpsize <- ceiling(1e7/26^2)
tt <- system.time(DF <-data.frame(
  x=rep(LETTERS, each=26*grpsize),
  y=rep(letters, each=grpsize), 
  v=runif(grpsize*26^2),
  stringAsFactors = FALSE)
)
tt
head(DF, 3)
tail(DF, 3)
dim(DF)
10000068/3.323
tt <-system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",])
tt

head(ans1, 3)
dim(ans1)

DT <-data.table(DF)
setkey(DT, x,y)
ss <- system.time(ans2<-DT[J("R","h")])
ss
head(ans2, 3)
dim(ans2)

# Bad case for using data.table
system.time(ans2 <- DF[DF$x=="R" & DF$y=="h",])
mapply(identical, ans1, ans2)

DT[,sum(v)]
options(digits=5)

head(DT)
DT[, sum(v), by=x]

ttt <- system.time(tt<-tapply(DT$v, DT$x, sum));ttt
tt

sss <- system.time(ss<-DT[, sum(v), by=x]);sss
ss

identical(as.vector(tt), ss$V1)
names(tt)
mode(tt)
class(tt)
class(ss)

sss<-system.time(ss<-DT[,sum(v), by="x,y"]);sss;ss

# Handling missing values & outliers
# 1st. 데이터 탐색
head(iris)
str(iris) # 데이터 구조 파악
summary(iris) # 기초 통계량
# 연속형 변수인 경우, 공분산 행렬과 상관계수행렬을 출력, 변수 간의 선형관계 파악
cov(iris[,1:4]) # 공분산
cor(iris[,1:4]) # 상관계수
summary(DT)

# 2nd. Missing data
# Generally, remove, replace and imputation
#Amelia, Mice, mistools
y <- c(1,2,3,NA)
is.na(y)
mydat[mydat$v1 == 99, "v1"] <- NA
head(mydat)
head(DT)
head(DF)
DT[DT$x=="A" & DT$y=="a", "v"] <- NA
is.na(DT)

# 2-1. remove NA
x <- c(1, 2, NA, 3)
mean(x)
mean(x, na.rm=T)
#complete.case()
mydat[!complete.cases(mydat),]

# Imputation with Amelia
install.packages("Amelia")
library(Amelia)
data(freetrade)
head(freetrade)
str(freetrade)

a.out <-amelia(freetrade, m=5, ts='year', cs='country')
a.out
head(a.out)
hist(a.out$imputation[[3]]$tariff, col="grey", border="white")
save(a.out, file="imputation.RData")
write.amelia(obj=a.out, file.stem='outdata')
missmap(a.out)

freetrade$tariff <-a.out$imputation[[5]]$tariff
missmap(freetrade)

# Remove NULL values from list
lst <- list(NULL, 1,2,3,4,5)
lst
sapply(lst, is.null)
lst[sapply(lst, is.null)] <-NULL
lst


# Outliers - 시간 많이 쓰지 않음
# 1st. Summary를 통해 제 1 / 3 사분위수로 1 차 판단
# 2nd. 주요 변수 별로 box plot
x <- rnorm(100)
boxplot(x)

x<-c(x, 19, 28, 30)
outwith <- boxplot(x)
outwith$out

# outliers 패키지
install.packages("outliers")
library(outliers)
set.seed(1234)
y=rnorm(100)
outlier(y)
outlier(y, opposite=TRUE)
dim(y) <-c(20,5)
outlier(y)
outlier(y, opposite=TRUE)
boxplot(y)
