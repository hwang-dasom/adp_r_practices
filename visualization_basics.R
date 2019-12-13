# Visualization

#ggplot2
install.packages("ggplot2")
library(ggplot2)

data(ChickWeight)
head(ChickWeight)

ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet, group=Chick)) + geom_line()
# geom_point() : scatter plot, geom_smooth(): 배경색상 투명도(alpah), 평균 값 선 굵기(size) 조정
ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet)) + geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1)

# point graph
h <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))
h+geom_point(alpha=0.3)

# smooth graph
h <- ggplot(ChickWeight, aes(x=Time, y=weight, colour=Diet))
h + geom_smooth(alpha=0.4, size=3)

# Histogram
# Time=21일 때, 무게 분포도
ggplot(subset(ChickWeight, Time==21), aes(x=weight, colour=Diet)) + geom_density()

h <- ggplot(subset(ChickWeight, Time==21), aes(x=weight, fill=Diet))
# facet_grid(Diet~.) : 가로 / facet_grid(.~Diet) : 세로
h + geom_histogram(colour='black', binwidth=50) + facet_grid(Diet~.)
h + geom_histogram(colour='black', binwidth=50) + facet_grid(.~Diet)

# Point Graph
data(mtcars)
head(mtcars)
help(mtcars)

p <- qplot(wt, mpg, colour = hp, data=mtcars)
p + coord_cartesian(ylim = c(0,40)) # y 축 range
p + scale_colour_continuous(breaks = c(100,300)) # hp range
p + guides(colour = "colourbar")

m <- mtcars[1:10, ]
p%+%m # 10개 데이터 무게별 연비 그래프

# Bar graph
c <- ggplot(mtcars, aes(factor(cyl)))
c + geom_bar()
c + geom_bar(fill="red") # 내부색 바꾸기
c + geom_bar(colour = "red") # line color
c + geom_bar(fill="white", colour="red")

k <- ggplot(mtcars, aes(factor(cyl), fill=factor(vs))) # fill option
k + geom_bar()

# FIXME : !Data 없음!
#m <- ggplot(movies, aes(x=rating))
# m + geom_histogram()
# m + geom_histogram(aes(fill= ..count..))

# Line graph
data(economics)
head(economics)

b <- ggplot(economics, aes(x = date, y=unemploy))
b + geom_line()
b + geom_line(colour = "red")
b + geom_line(colour = 'red', size = 3)
b + geom_line(linetype=2)

b + geom_line(linetype=1)
b + geom_line(linetype=2)
b + geom_line(linetype=3)
b + geom_line(linetype=4)

# 효과주기
df <- data.frame(x =rnorm(5000), y = rnorm(5000))

h <- ggplot(df, aes(x,y))
h + geom_point()
h + geom_point(alpha = 0.5)
h + geom_point(alpha = 1/10)

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(size=4)
p + geom_point(aes(colour=factor(cyl)), size=4)

p + geom_point(aes(shape=factor(cyl)), size=4)

# reshape2, plyr
install.packages("reshape2")
install.packages("pylr")

library(reshape2)
library(plyr)

rescale01 <- function(x) (x-min(x)) / diff(range(x))
ec_scaled <- data.frame(date = economics$date, colwise(rescale01)(economics[, -(1:2)]))
ecm <- melt(ec_scaled, id = "date")

f <- ggplot(ecm, aes(date, value)) 
f + geom_line(aes(linetype = variable))

data(diamonds)
head(diamonds)

k <- ggplot(diamonds, aes(carat, ..density..)) + geom_histogram(binwidth=0.2)
k + facet_grid(.~cut)

w <- ggplot(diamonds, aes(clarity, fill=cut))
w + geom_bar()
# w + geom_bar(aes(order = desc(cut)))

df <- data.frame(x=1:10, y=1:10)
f <- ggplot(df, aes(x=x, y=y)) 
f + geom_line(linetype=2)
f+ geom_line(linetype = "dotdash")

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(size=4)
p + geom_point(aes(size=qsec)) # qsec별 point 크기 다름

# Ponint + Line(임의)
p + geom_point(size=2.5) + geom_hline(yintercept = 25, size=3.5)

p + geom_point()
p + geom_point(shape=5)
p + geom_point(shape="k", size=3)
p + geom_point(shape=".")
p + geom_point(shape=NA)

# Various shapes
df2 <- data.frame(x = 1:5, y=1:25, z= 1:25)

s <- ggplot(df2, aes(x=x, y=y))
s+ geom_point(aes(shape=z), size=4) + scale_shape_identity()

# point range graph
dmod <- lm(price ~ cut, data=diamonds)
cuts <- data.frame(cut = unique(diamonds$cut), 
                   predict(dmod, data.frame(cut = unique(diamonds$cut)),
                           se=TRUE)[c("fit", "se.fit")])

se <- ggplot(cuts, aes(x=cut, y = fit, ymin=fit-se.fit,
                       ymax = fit+se.fit, colour = cut))
se + geom_pointrange()

# 특정 영역 강조
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p + annotate("rect", xmin=2, xmax=3.5, ymin = 2, ymax=25, 
             fill="black", alpha=.6)

# qplot + smooth
p <- qplot(disp, wt, data=mtcars) + geom_smooth()
p

# limits 원하는 부분만
p + scale_x_continuous(limits = c(325, 500))

# 확대
d <- ggplot(diamonds, aes(carat, price))
d + stat_bin2d(bins=25, colour="grey50")
d + stat_bin2d(bins=25, colour="grey50")+scale_x_continuous(limits=c(0,2))

# boxplot
qplot(cut, price, data=diamonds, geom="boxplot") # 세로
last_plot() + coord_flip() # 가로
qplot(cut, data = diamonds, geom="bar")


h <- qplot(carat, data=diamonds, geom="histogram")
h + coord_flip()
h + coord_flip() + scale_x_reverse()

# Mutiple Axis
# multiple y axis in plot.R
time <- seq(7000, 3400, -200) 
pop <- c(200, 400, 450, 500, 300, 100, 400, 700, 830, 1200, 400,
         350, 200, 700, 370, 800, 200, 100, 120)
grp <- c(2,5,8,3,2,2,4,7,9,4,4,2,2,7,5,12,5,4,4)
med <- c(1.2, 1.3, 1.2, 0.9, 2.1, 1.4, 2.9, 3.4, 2.1, 
         1.1, 1.2, 1.5, 1.2, 0.9, 0.5, 3.3, 2.2, 1.1, 1.2)
par(mar=c(5,12,4,4)+0.1)

plot(time, pop, axes=F, ylim=c(0, max(pop)), xlab="", ylab="", 
     type="l", col="black",main="", xlim=c(7000, 3400))

points(time, pop, pch=20, col="black")

axis(2, ylim=c(0, max(pop)), col="black", lwd=2)
mtext(2, text="Population", line=2)

par(new=T)
plot(time, med, axes=F, ylim=c(0, max(med)), xlab="", ylab="",
     type="l", lty=2, main="", xlim=c(7000, 3400), lwd=2)
axis(2, ylim=c(0, max(med)), lwd=2, line=3.5)
points(time, med, pch=20)
mtext(2, text="Median Group Size", line=5.5)

par(new=T)
plot(time, grp, axes=F, ylim=c(0, max(grp)), xlab="", ylab="",
     type="l", lty=3, main="", xlim=c(7000, 3400), lwd=2)
axis(2, ylim=c(0, max(grp)), lwd=2, line=7)
points(time, grp, pch=20)
mtext(2, text="Number of Groups", line=9)

axis(1, pretty(range(time), 10))
mtext("cal BP", side=1, col="black", line=2)

legend(x=7000, y=12, legend=c("Population", "Median Group Size", "Number of Groups"), lty=c(1,2,3))
