library('dplyr')
library('psych')
library('ggplot2')
save(data, file = 'data/data.rda')
load("data/data.rda")
data
save(mpg, file = 'data/mpg.rda')
load("data/mpg.rda")
rm(mpg)
data
#1-1
#(1) 데이터준비
mnmath = data %>% filter(cls %in% c('죽', '매')) %>% select(cls, math)
mnmath;
mnmath$cls = factor(mnmath$cls, levels=c('죽','매'), labels=c('죽', '매'))
mnmath$cls
describeBy(mnmath$math, mnmath$cls, mat = T)

orgpar = par(no.readonly = T)
#(2) 데이터 확인

boxplot(mnmath$math ~ mnmath$cls)
layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
boxplot(mnmath$math ~ mnmath$cls)
hist(mnmath$math[mnmath$cls == '죽'])
hist(mnmath$math[mnmath$cls == '매'])
par(orgpar)

#(3) 등분산 검정
var.test(mnmath$math ~ mnmath$cls, data = mnmath)


#(4) t-test 수행

t.test(mnmath$math ~ mnmath$cls, data = mnmath,
       alternative = c("two.sided"),
       var.equal = T,                 # 등분산검증의 p-value < 0.05 이면 False로!
       conf.level = 0.95)

#(5) 결과 그래프

mu = 63.4; se = 2.144661; rn = sort(rnorm(1000, mu, se))
plot(rn, dnorm(rn, mu, se), col='green', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25)) 
abline(v=mu, col="green", lty=5)
par(new = T)  

mu = 63.84; se = 2.114145; rn = sort(rnorm(1000, mu, se))
plot(rn, dnorm(rn, mu, se), col='red', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25))
abline(v=mu, col="red", lty=5)


# 1-2
#(1)
#(2) 
describeBy(data$math, data$cls, mat = T)

#(3) 그래프로 확인하기
ggplot(data, aes(x=cls, y=math)) +
  geom_boxplot(outlier.color = 'blue') +
  ggtitle("각반 수학 성적")

ggplot(data, aes(x=math)) +
  geom_histogram(binwidth = 10, col='white') +
  facet_grid(. ~ data$cls)   # 그룹별로 그려라!

#(4) 등분산(분산의 동질성) 검정 (p-value > 0.05 면 등분산)
bartlett.test(data$math ~ data$cls, data=data)  # ⇒ p-value = 0.8497 ⇒ 약 85% 동질하다

aaa = aov(data$math ~ data$cls, data=data)
summary(aaa)   

TukeyHSD(aaa)

plot(TukeyHSD(aaa)) 

#(5) 결과 그래프
draw = function(rn, mu, se, col) {
  plot(rn, dnorm(rn, mu, se), col=col, type = 'l',
       xlim = c(50, 80), ylim = c(0, 0.25))
  abline(v=mu, col=col, lty=5)
}

mu = 63.59; se = 2.020535; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'red')
par(new = T)
mu = 63.08; se = 2.028632; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'blue')
par(new = T)
mu = 63.84; se = 2.114145; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'green')
par(new = T)
mu = 63.47; se = 2.144661; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'black')

legend('topright',
       legend=c('국', '난', '매', '죽'),
       pch=8,
       col=c('red', 'blue', 'green', 'black'),
       bg='gray')

#2-1
#(1) 데이터 준비
ke = data %>% select(korean,english)
#(2) 기술통계 확인
describe(ke)
#(3) 그래프로 데이터 확인하기
pairs.panels(ke)
#(4) 상관분석
cor(ke, use = "complete.obs", method = c("pearson")) 
#(5) 결과 그래프
plot(korean ~ english, data=ke)
abline(lm(korean ~ english, data=ke), col='red')

#2-2
#(1) 데이터 준비
unique(mpg$trans)
unique(mpg$year)
lrmpg = mpg %>%
  mutate(trs = ifelse(substr(trans, 1, 4) == 'auto', 1, 0), 
         y = ifelse(year == 1999, 0, 1)) %>%
  select(y, displ, cyl, trs, cty, hwy)

#(2) 기본 통계치 확인
describe(lrmpg)
pairs.panels(lrmpg)
#(3) 분석
glmdmpg = glm(y ~ displ+cyl+cty+hwy+trs, family = binomial, data=lrmpg)
summary(glmdmpg)  # Estimate: 기울기(비례/반비례), Pr: 0.05보다 작으면 영향이 있다
plot(glmdmpg)

#(4) coefficients(기울기+절편)와 confint(신뢰구간)로 LOR(Log Odd Ratio) 구하기
round(exp(cbind(LOR = coef(glmdmpg), confint(glmdmpg))), 2)

#따라서 배기량(displ)이 고객 만족도에 가장 큰 영향을 미쳤다.

