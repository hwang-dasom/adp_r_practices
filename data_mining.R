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
