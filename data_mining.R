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

# setosa = 1, versicolor = 2, 큰 숫자에 오즈 모형화함 / binomial : 정규화분포
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

# Null 모형에서 mpg, am이 차례로 추가됨에 따라 발생하는 이탈도의 감소량 제시
anova(glm.vs, test="Chisq")
1-pchisq(18.327, 1) # == anova 결과 p-value: P(X^2(1) > 18.327)
1-pchisq(4.887, 1) # == anova 결과 p-value: P(X^2(1) > 4.887)

#[Example 2] Neural network model (NN model)
# + : 변수 많을 때, 입, 출력변수 복잡한 비선형관계일 때 유용, 잡음에 강함
# - : 결과 해석 쉽지않음, 은닉층 수& 노드 수 결정 어렵, 초기값에 따라 전역해 보다 지역해수렴 가능성

# A. package {nnet} - nnet() : 1-hidden layer
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

# B. {neuralnet} package - neuralnet() : 1-hidden layer
install.packages("neuralnet")
library(neuralnet)

iris.net <- neuralnet(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris, 
                      hidden = 2, err.fct="ce", linear.output = FALSE, likelihood=TRUE)
iris.net
plot(iris.net)

data(infert, packages="datasets") # warning 뜸
str(infert)
# hidden: # of hidden neurons, err.fct: sse(the sum of squaure), ce(the cross-entropy) 
# linear.output: applied act.fct(FALSE), otherwise, TRUE, likelyhood: ? 
# case: 1- 사례 / 0 - 대조
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

net.infert2 <- neuralnet(case~parity+induced+spontaneous, data=infert, 
                         hidden=2, err.fct = "ce", linear.output = FALSE, likelihood = TRUE)
plot(net.infert2)

new.output <- compute(net.infert, 
                      covariate = matrix(c(22,1,0,0,
                                            22,1,1,0,
                                            22,1,0,1,
                                            22,1,1,1), byrow=TRUE, ncol=4))
new.output$net.result

new.output2 <- compute(net.infert2, covariate=matrix(c(1,0,0,
                                                       1,1,0,
                                                       1,0,1,
                                                       1,1,1), byrow=TRUE, ncol=3))
new.output2$net.result

# weight 신뢰구간
confidence.interval(net.infert, alpha=0.05)
confidence.interval(net.infert2, alpha=0.05)

# C. multiple hidden layers
# 1-100 사이의 난수 50개 발생, 제곱근 취한 값을 자료로 구축 => 신경망 학습 => 새로운 자료 예측
set.seed(1)
train.input <- as.data.frame(runif(50, min=0, max=100))
train.output <- sqrt(train.input)
train.data <- cbind(train.input, train.output)
colnames(train.data) <- c("Input", "Output")
head(train.data)

# 1-hidden layer, 10 nodes, threshold: 오차함수 편미분 값 - 정지규칙(stopping rule)
net.sqrt <- neuralnet(Output~Input, train.data, hidden=10, threshold=0.01)
print(net.sqrt)
plot(net.sqrt)

test.data <- as.data.frame((1:10)^2)
test.out <- compute(net.sqrt, test.data)
ls(test.out)
print(test.out$net.result)

# 2-hidden layer, 10, 8 nodes each
net2.sqrt <- neuralnet(Output~Input, train.data, hidden=c(10,8), threshold=0.01)
plot(net2.sqrt)
test2.out <- compute(net2.sqrt, test.data)
print(test2.out$net.result)


# 의사결정나무 모형(Decision tree)
# + : 1. 구조 단순, 해석 용이 2. 수학적 가정이 불필요한 비모수적 모형 
#     3. 유용한 입력 변수의 파악과 예측변수간의 상호작용 및 비선형성으 고려하여 분석 수행
# - : 1. 연속성데이터 -> 비연속성 경계값에 오차가 큼 2. 각 예측변수의 효과 파악 어렵
#     3. 새로운 자료에 대한 예측 불안정 가능성
#활용분야: 고객타겟팅, 신용점수화, 캠페인 반응분석, 고객행동예측, 고객 세분화
# 분류나무 - 목표변수 이산형
# 회귀나무 - 목표변수 연속형
# Example 1. iris 자료 활용한 의사결정 나무 분석
install.packages("rpart")
library(rpart)
head(iris)
iris <- iris[1:5]
iris
c <- rpart(Species~., data=iris)
c
# 시각화 1(Decision tree visualization 1)
plot(c, compress=T, margin=0.3)
text(c, cex=1.5)

head(predict(c, newdata=iris, type="class"))
tail(predict(c, newdata=iris, type="class"))

# 시각화2 (Decision tree visualization 2 : 더 선호함)
install.packages("rpart.plot")
library(rpart.plot)
prp(c, type=4, extra=1) 
prp(c, type=4, extra=2) # 자료해석: 49/54 -> 해당 노드에 54개 분류, 그 중에 49개가 versicolor

# 가지치기(pruning) 예제
# 트리 크기에 따른 cost-complexity parameter(비용-복잡도 모수) & cross-validation error(교차타당성 오차)
c$cptable
opt <- which.min(c$cptable[,"xerror"])
cp <- c$cptable[opt, "CP"] 
prune.c <- prune(c, cp=cp)
plot(prune.c)
text(prune.c, use.n=F)

prp(prune.c, type=4, extra=2)

#cp 값 그리기
plotcp(c) # {rpart} 패키지

# Example 2. 패키지 {party} - ctree()
# 146명 환자데이터, 7개 예측변수 -> 범주형 반응변수(ploidy) 예측 또는 분류
install.packages("party")
library(party)
str(stagec)

stagec1 <- subset(stagec, !is.na(g2))
stagec2 <- subset(stagec1, !is.na(gleason))
stagec3 <- subset(stagec2, !is.na(eet))
str(stagec3)

set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(0.7, 0.3)) # 7:3 index
ind
trainData <- stagec3[ind==1, ] # .7 ind -> train data
testData <- stagec3[ind==2, ] # .3 ind -> test data 

tree <- ctree(ploidy ~., data=trainData) # train data로 적합
tree
# visualization
plot(tree)

testPred = predict(tree, newdata = testData)
table(testPred, testData$ploidy) # 열이름: 실제값, 행이름: 결과

# Example 3. ctree() 함수 - 연속형 반응변수 => 회귀나무
# airquality 자료로 의사결정나무모형 적합, Ozone 결측인 자료 제외 후, ctree()
airq <- subset(airquality, !is.na(Ozone))
airq <- subset(airq, !is.na(Solar.R))

head(airq)
airct <- ctree(Ozone ~., data=airq)
airct
plot(airct)

head(predict(airct, data=airq))
predict(airct, data=airq, type="node") # 속하는 노드 번호가 나옴, where(airct)의 결과와 동일
mean((airq$Ozone - predict(airct))^2) # 평균오차제곱


# 앙상블 (Ensemble)
# Bagging example {adabag}'s bagging
install.packages("adabag")
library(adabag)
data(iris)
iris.bagging <- bagging(Species~., data=iris, mfinal=10) # mfinal: # of iterations
iris.bagging$importance
plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

iris.pred <- predict(iris.bagging, newdata=iris)
boos.tb <- table(iris.pred$class, iris[,5])

#Boosting example 1. {adabag}'s boosting()
boo.adabag <- boosting(Species~., data=iris, boos=TRUE, mfinal=10)
boo.adabag$importance

plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])
boo.pred <- predict(boo.adabag, newdata=iris)
tb <- table(boo.pred$class, iris[,5])
tb

# 오분류율
boos.error.rpart <- 1-(sum(diag(tb))/sum(tb))
boos.error.rpart

bag.error.rpart <- 1-(sum(diag(boos.tb))/sum(boos.tb))
bag.error.rpart

# Boosting example 2. {dad}'s ada()
install.packages("ada")
library(ada)

iris[iris$Species!='setosa', ] -> iris # 'setosa' 데이터 제외
n <- dim(iris)[1]
n
dim(iris)

tr.idx <- sample(1:n, floor(.6*n), FALSE)
te.idx <- sample(1:n, tr.idx)
iris[, 5] <- as.factor((levels(iris[, 5])[2:3])[as.numeric(iris[, 5])-1])

gdis <- ada(Species~., data=iris[tr.idx,], iter=20, nu=1, type="discrete")
gdis <- addtest(gdis, iris[te.idx, -5], iris[te.idx, 5])
gdis

# 오차와 일치도를 나타내는 카파(kappa)계수 그림
plot(gdis, TRUE, TRUE) # train / test둘다
varplot(gdis) # 변수의 중요도
pairs(gdis, iris[tr.idx, -5], maxvar=4)


# Random Forest example 1. {randomForest}'s randomForest()
install.packages("randomForest")
library(randomForest)
library(rpart)

#Data 준비
data(stagec)
stagec3 <- stagec[complete.cases(stagec), ] # complete.cases() : dealing with missing values
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(0.7, 0.3))
trainData <- stagec3[ind==1,]
nrow(trainData)
testData <- stagec3[ind==2, ]
nrow(testData)
# 모형 생성
rf <- randomForest(ploidy~., data=trainData, ntree=100, proximity=TRUE)

#결과
table(predict(rf), trainData$ploidy)
print(rf) # print Confusion Matrix, OOB(Out-of-bag) 추정치
plot(rf) # 종속변수의 범주별 오분류율 (검은색 - 전체)
importance(rf) # 변수의 중요성
varImpPlot(rf) # 불순도 감소값 - 클수록 순수도 증가
plot(margin(rf)) # 양의 값(+) : 정확한 분류 / 음의 값(-) : 잘못된 분류

#예측
rf.pred <- predict(rf, newdata=testData)
table(rf.pred, testData$ploidy)

# 2. {party}'s cforest()
library(party)
set.seed(1234)
cf <- cforest(ploidy~., data=trainData)
table(predict(cf), trainData$ploidy)
cf.pred <- predict(cf, newdata=testData, OOB=TRUE, type="response")
table(cf.pred, testData$ploidy)

# 모형평가 (Model Evaluation)
# 1. hold-out : sample() - 전체의 70% 훈련용, 30% 모데 검증용으로 사용
data(iris)
nrow(iris)
set.seed(1234)
idx <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7, 0.3)) # 1. train data, 2. test data
trainData <- iris[idx==1, ]
testData <- iris[idx==2, ]
nrow(trainData)
nrow(testData)

# 2. 교차검증, Cross validation을 위해 데이터 추출
data(iris)
set.seed(1234)
k = 10 # 10-fold cross validation
iris <- iris[sample(nrow(iris)), ] # randomly suffle the data
folds <- cut(seq(1, nrow(iris)), breaks=k, labels=F)
trainData = list(0)
testData = list(0)
for(i in 1:k){
  testIdx <- which(folds==i, arr.ind=TRUE)
  testData[[i]] <- iris[testIdx, ]
  trainData[[i]]<- iris[-testIdx, ]
}
head(trainData[[1]])
head(trainData[[2]])

# 성능평가 
# 범주형 변수 1. 오분류표 2. ROC 그래프 3. 이익 도표 4. 향상도 곡선
# 1. 오분류표
# iris 일부 데이터를 이용하여 범주가 2개인 분류 모형 구축, 70%의 훈련용 자료 추출
data(iris)
iris.sub <- subset(iris, Species=="setosa" | Species=="versicolor")
iris.sub$Species <- factor(iris.sub$Species)
set.seed(1234)
iris.sub <- iris.sub[sample(nrow(iris)),] # Randomly shuffle the data
trainData <- iris.sub[1:(nrow(iris)*0.7),]
testData <- iris.sub[((nrow(iris)*0.7)+1):nrow(iris),]
nrow(trainData)

# 모형학습: {nnet}'s nnet(), {rpart}'s rpart()
library(nnet)
library(rpart)

# neural network
nn.iris <- nnet(Species~., data=trainData, size=2, rang=0.1, decay=5e-4, maxit=200)

# decision tree
dt.iris <- rpart(Species~., data=trainData)

# 검증용 자료
nn.pred <- predict(nn.iris, testData, type="class")
dt.pred <- predict(dt.iris, testData, type="class")

# FIXME: confusionMatrix() 함수에러 처리
#Error: `data` and `reference` should be factors with the same levels.
nn.pred <- factor(nn.pred) # 추가함

# confusion maxtrix : {caret}'s confusionMatirx()
install.packages("caret")
library(caret)
nn.con <- confusionMatrix(nn.pred, testData$Species)
dt.con <- confusionMatrix(dt.pred, testData$Species)
nn.con$table
dt.con$table

# 오분류표를 이용하여 대표적인 지표를 계산, 모형 결과 비교
# accurarcy - 정분류율
accuracy <- c(nn.con$overall['Accuracy'], dt.con$overall['Accuracy'])
# precision - 정확도
precision <- c(nn.con$byClass['Pos Pred Value'], dt.con$byClass['Pos Pred Value'])
# recall - 재현율
recall <- c(nn.con$byClass['Sensitivity'], dt.con$byClass['Sensitivity'])
# F1 지표
f1 <- 2*((precision * recall)/ (precision + recall))

result <- data.frame(rbind(accuracy, precision, recall, f1))
names(result) <-c("Nerual Network", "Decision Tree")
result

# 2. ROC graph
# Infert 자료 분류 분석 모형 평가
# Decision tree : {C50}' C5.0(), Neural net: {neuralnet}'s neuralnet()
# data preparation
set.seed(1234)
data(infert)
infert <- infert[sample(nrow(infert)), ] # Shuffle
infert <- infert[,c("age", "parity", "induced", "spontaneous", "case")]
trainData <- infert[1:nrow(infert)* 0.7,]
testData <- infert[(nrow(infert)*0.7+1): nrow(infert),]

# NN model
library(neuralnet)
net.infert <- neuralnet(case~age+parity+induced+spontaneous, data=trainData, hidden=3, err.fct="ce", 
                        linear.output=FALSE, likelihood = TRUE)
n_test <- subset(testData, select=-case)
nn_pred <- compute(net.infert, n_test)
testData$net_pred <- nn_pred$net.result
head(testData)

# DT model
install.packages("C50")
library(C50)
trainData$case <- factor(trainData$case)
dt.infert <- C5.0(case~age+parity+induced+spontaneous, data=trainData)
testData$dt_pred <- predict(dt.infert, testData, type="prob")[,2]
head(testData)

# ROC graph {Epi}'s ROC()
install.packages("Epi")
library(Epi)
#FIXME: Error in m[, 3] : subscript out of bounds
neural_ROC <- ROC(form=case~net_pred, data=testData, plot="ROC")
#FIXME: Error in m[, 3] : subscript out of bounds
dtree_ROC <- ROC(form=case~dt_pred, data=testData, plot="ROC")

# 3. 이익 도표 & 4. 향상도 곡선 {ROCR}'s performance()
install.packages("ROCR")
library(ROCR)
# FIXME: Error in prediction(testData$net_pred, testData$case) : 
#         Number of classes is not equal to 2.
#         ROCR currently supports only evaluation of binary classification tasks.
n_r <- prediction(testData$net_pred, testData$case)
d_r <- prediction(testData$dt_pred, testData$case)
n_p <- performance(n_r, "tpr", "fpr") # ROC for NN
d_p <- performance(d_r, "tpr", "fpr") # ROC for DT
plot(n_p, col="red") # NN - red
par(new=TRUE)
plot(d_p, col="blue") # DT - blue
abline(a=0, b=1) # random model - black
