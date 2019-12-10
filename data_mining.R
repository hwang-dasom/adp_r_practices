# Data Mining
# 1. Classification : 반응 변수 범주형 - 분류 / 연속형 - 예측
# 2. Estimation
# 3. Prediction
# 4. Association Analysis
# 5. Clustering
# 6. Description

# 1. Classification
# 로지스틱 회귀, 신경망, 의사결정나무, 앙상블, 규칙기반, 
# 사례기반, 인접이웃, 베이즈 분류, SVM, 유전자

#[Exmaple 1]
# 로지스틱 회귀모형 : 반응변수가 범주형/ 예측 또는 분류
# 사전정보 또는 손실함수 이용, 정분류율, 민감도, 특이도 등 고려
data(iris)
a <- subset(iris, Species=="setosa" | Species == "versicolor")
a$Species <- factor(a$Species)
str(a)
a

# setosa = 1, versicolor = 2, 큰 숫자에 오즈 모형화함
b <- glm(Species~Sepal.Length, data=a, family=binomial)
# versicolor: 귀무가설 / setosa: 대립가설
summary(b)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -27.83       5.43   -5.12  3.0e-07 ***
#  Sepal.Length     >>"5.14"<<       1.01    5.11  3.3e-07 ***
# Sepal.Length 가 증가할 수록, Versicolor(Y=2)일 오즈가 exp(5.140)-약 170 배 증가
# Null deviance: 138.629  on 99  degrees of freedom
# -> 절편만 포함하는 모형(H0: 베타=0하의 모형)의 완전모형으로부터의 이탈도
# p-value : P(X^2(99) > 138.629) = 0.005 통계적 유의
# Residual deviance:  64.211  on 98  degrees of freedom
# Sepal.Length 추가 적합 모형 이탈도
# 자유도 1 기준에 이탈도 감소 74.418(138.629 - 64.211)
# p-value : p(x^2(98)>64.211) = 0.997 -> 대립 가설 기각

coef(b)
exp(coef(b)["Sepal.Length"])

#회귀계수 베타 신뢰구간
confint(b, parm="Sepal.Length")
# 오즈의 증가량 exp(b-베타) 신뢰구간
exp(confint(b, parm="Sepal.Length"))

#적합결과
fitted(b) [c(1:5, 96:100)]

#새로우 자료에 대한 예측
predict(b, newdata=a[c(1, 50, 51, 100),], type="response")
# 연속형 변수(Sepal.Length)에 대한 범주형 변수(Species)의 조건부 분포
cdplot(Species~Sepal.Length, data=a)
#적합된 로지스틱스회귀 모형
plot(a$Sepal.Length, a$Species, xlab = "Sepal.Length")
x=seq(min(a$Sepal.Length), max(a$Sepal.Length), 0.1)
lines(x, 1+(1/(1+(1/exp(-27.831+5.140*x)))), type="l", col="red")

#[Example 2]
attach(mtcars)
str(mtcars)
glm.vs <- glm(vs~mpg+am, data=mtcars, family=binomial)
summary(glm.vs)

step.vs <- step(glm.vs, direction="backward")

summary(step.vs)
ls(glm.vs)
str(glm.vs)

anova(glm.vs, test="Chisq")
1-pchisq(18.327, 1)
1-pchisq(4.887, 1)

#[Example 2] Neural network model (NN model)
# A. package {nnet} - nnet()
install.packages("nnet")
library(nnet)
# size: # of units in hidden layer, rang: initial random weights on[-rang, rang]
# decay: parameter for weight decay, maxit: maximum # of iterations.
nn.iris <- nnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris, size=2, rang=0.1, decay=5e-4, maxit=200)
summary(nn.iris)

#적합결과 시각화예제 1
install.packages("devtools")
library(devtools)
source('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nn.iris)

#적합결과 시각화 예제 2.
install.packages("clusterGeneration")
install.packages("scales")
library(clusterGeneration)
library(scales)
library(reshape)
plot(nn.iris)

#confusion matix: 정오분류표
table(iris$Species, predict(nn.iris, iris, type="class"))

# B. {neuralnet} package - neuralnet()
install.packages("neuralnet")
library(neuralnet)

data(infert, packages="datasets") # warning 뜸
str(infert)
# hidden: # of hidden neurons, err.fct: sse(the sum of squaure), ce(the cross-entropy) 
# linear.output: applied act.fct(FALSE), otherwise, TRUE, likelyhood: ? 
net.infert <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=2,
                        err.fct="ce", linear.output = FALSE, likelihood=TRUE)
net.infert

plot(net.infert)
# 함수 결과 '열' 확인
names(net.infert)
# 결과 행렬
net.infert$result.matrix
# 전제자료는 $data
# 모형 적합에 사용된 자료 $covariate, $response
# 적합값 $net.result
# 초기 가중치 $startweights, 적합 가중치 $weights
out <-cbind(net.infert$covariate, net.infert$net.result[[1]])
dimnames(out) <- list(NULL, c("age", "parity", "induced", "spontaneous", "nn-output"))
head(out)
head(net.infert$generalized.weights[[1]])
par(mfrow=c(2,2))
gwplot(net.infert, selected.covariate = "age", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "parity", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "induced", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "spontaneous", min=-2.5, max=5)
par(mfrow=c(1,1))
# 공변량 age만 0 근처에 그래프 그려짐
# age 제외, parity, induced, spontaneous 로 신경망모형 적합 가능

new.output <- compute(net.infert, 
                      covariate = matrix(c(22,1,0,0,
                                            22,1,1,0,
                                            22,1,0,1,
                                            22,1,1,1), byrow=TRUE, ncol=4))
new.output$net.result
