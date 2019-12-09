#Exeries
# 1. 기초 통계량
# 2. profits에 따른 CEO salary에 대한 산점도를 그리고 상관계수를 구하시오.
# 3. profits에 따른 CEO salary에 대한 단순선형회귀식 구하고 분석
# 4. profits과 age, sales에 따른 CEO의 Salary의 회귀식을 구하고 분석
# 5. profits, age, sales, tenure, assets에 따른 CEO의 salary의 회귀식으 구하고 분석
# 6-8. 후진제거법 / 전진선택법 / 단계적 방법으로 최적의 회귀식 찾기 (종속변수: salary)

#data
salary <- c(3030, 6050, 3571, 3300, 0, 9375, 9525, 5000, 999, 3300, 
            3500, 2493, 1911, 2130, 1185, 5236, 1990, 6000, 6229, 1523)
tenure <- c(7, 0, 11, 6, 18, 6, 15, 5, 3,2, 16, 5, 7, 4, 0 ,2, 4, 32, 5, 3)
age <- c(61, 51, 63, 60, 63, 57, 60, 61, 57, 60, 63, 61, 58, 59, 56, 60, 60, 74, 63, 56)
sales <- c(161315, 144416, 139208, 100697, 100469, 81667, 76431, 57813, 56154, 53588,
           50777, 47678, 47061, 41322, 37154, 35853, 33674, 33296, 32379, 31707)
profits <- c(2956, 22071, 4430, 6370, 9296, 6328, 5807, 5372, 1120, 6398, 5165, 1704, 2945, 
             1048, 3780, 1259, 568, 3765, 3782, 578)
assets <- c(257389, 237545, 49271, 92630, 355935, 86100, 668641, 58820, 36672, 59550, 617679,
            42754, 33673, 37675, 30966, 299804, 14166, 19166, 194398, 3665875)
firm <- data.frame(salary, tenure, age, sales, profits, assets)
firm
#1. 기초 통계량
summary(firm) # 평균, 중앙값, 최댓값, 최솟값
for(i in 1:ncol(firm)){
  print(colnames(firm)[i])
  var <- var(firm[,i])
  sd <- sd(firm[, i])
  print(var)
  print(sd)
}

# 2. profits에 따른 CEO salary에 대한 산점도를 그리고 상관계수를 구하시오.
plot(firm$profits, firm$salary)
cor(firm$profits, firm$salary)
# > 0.30196

# 3. profits에 따른 CEO salary에 대한 단순선형회귀식 구하고 분석
res <- lm(firm$salary~firm$profits, data=firm)
res
summary(res)
# > salary = 0.167*profits + 3024.111
# > p-value: 0.196으로 추정된 회귀식이 통계적으로 유의하지 않음

# 4. profits과 age, sales에 따른 CEO의 Salary의 회귀식을 구하고 분석
res2 <- lm(firm$salary~firm$profits+firm$age+firm$sales, data=firm)
res2
summary(res2)
# salary = 0.234 * profits + 0.0127 * age - 0.00565 * sales - 0.00457
# p-value: 0.486으로 회귀식이 통계적으로 유의하지 않음

# 5. profits, age, sales, tenure, assets에 따른 CEO의 salary의 회귀식으 구하고 분석
res3 <- lm(firm$salary ~ firm$profits+firm$age+firm$sales+firm$tenure+firm$assets, data=firm)
res3
summary(res3)
# salary = 0.207 * profits + 0.388 * age - 0.00635 * sales + 0.488 * tenure - 0.000194 * assets + 0.0631
# p-value : 0.795 추정식 통계적으로 유의하지 않음

# 6-8. 후진제거법 / 전진선택법 / 단계적 방법으로 최적의 회귀식 찾기 (종속변수: salary)
# 6. 후진제거법
back <- step(lm(firm$salary~., data=firm), direction="backward")
back
# > salary = 3817

# 7. 전진선택법
forward <- step(lm(firm$salary~1, data=firm), 
                scope=list(lower=~1, upper=~firm$tenure+firm$age+firm$sales+firm$profits+firm$assets)
                , direction="forward")
forward

#> salary = 3817

# 8. 단계적 방법
both <- step(lm(firm$salary~1, data=firm), 
             scope=list(lower=~1, upper=~firm$tenure+firm$age+firm$sales+firm$profits+firm$assets),
             direction = "both")
both
# > salary = 3817
