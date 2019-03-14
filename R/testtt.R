# arrange, mutate
select(filter(data, Math>95, English<60), Math, English)
head(arrange(data, desc(Math)))
data %>% arrange(Math) %>% head
data %>% arrange(desc(Math)) %>% head
data %>% arrange(Math, Korean,English) %>% head

# data %>% mutate(추가변수 = 식)

data %>% mutate(kor_eng=kor+eng) %>% arrange(desc(kor_eng)) %>% head
data = rename(data, kor=Korean, eng=English, math=Math)
data = data %>% mutate(subTotal = kor + eng + math)
head(data)

# summarize, group_by ####
data %>% summarize(t = mean(math))
class(data %>% summarize(t = mean(math)))
data %>% group_by(cls, gen) %>% summarise(m=mean(math))

data %>% 
  group_by(cls) %>%
  summarize(mean_math = mean(math),
            sum_math = sum(math),
            medi_math = median(math),
            n_math =  n()) %>%
  arrange(desc(mean_math))

# join ####

dfsum = cbind( data.frame(yno=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))))
dfsum
sales = cbind( data.frame(no=1:12, year=2016:2019), 
               matrix(round(runif(144), 3) * 100000, ncol=12, dimnames = list(NULL, month.abb)) )
sales

left_join(sales, dfsum, by=c('year' = 'year'))
right_join(sales, dfsum, by=c('year' = 'year'))

inner_join(sales, dfsum, by=c('year' = 'year', 'no'='yno'))
semi_join(sales, dfsum, by=c('year'='year', 'no'='yno'))
full_join(sales, dfsum, by=c('year' = 'year', 'no' = 'yno'))
anti_join(sales, dfsum, by=c('no' = 'yno'))

#bind_rows, bind_cols
topsale4 = sales[1:4,] %>% select(year, Jan, Apr, Jul, Oct)
top4 = sales[5:8,] %>% 
  select(1:4, year, Jan, Apr, Jul, Oct) %>% 
  rename(yno=no, Q1=Jan, Q2=Apr, Q3=Jul, Q4=Oct)
bind_rows(dfsum, topsale4)
bind_rows(dfsum, top4)
bind_rows(dfsum, top4, .id = 'group')

bind_cols(dfsum, top4)
cbind(dfsum, top4)
bind_cols(dfsum,top4) %>% select(-year1, -yno1, -Feb)

# Try This : dplyr ####
#1
mpg %>% 
  filter(class %in% c('suv','compact')) %>%
  select(model, fl)

#2
mpg %>%
  arrange(desc(hwy)) %>%
  head(5)

#3
mpg %>%
  filter(class == 'suv') %>%
  mutate(total_fl = (cty + hwy) / 2 ) %>%
  group_by(manufacturer) %>%
  summarise(avg = mean(total_fl)) %>%
  arrange(desc(avg)) %>%
  head(5)

#4
fp =cbind(data.frame(no=1:5),matrix(c('c','d','e','p','r', 'CNG','diesel', 'E85','Premi','Reqular',1.33,1.02,0.92,1.99,1.22),nrow=5,dimnames=list(NULL,c('fl', 'type', 'price'))), stringsAsFactors = F)
fp =as.data.frame(fp, stringsAsFactors = F)
fp
inner_join(mpg,fp , by=c('fl'='fl')) %>% select(-type)
as.character(fp$fl)

class(fp$fl)

# plots - par() ####
prePar = par(col='red')
plot(smdt)
par(prePar)
plot(data$eng)

# plot - basic plot ####
plot(x=1, y=1)
plot(x=1:10, y=1:10)
plot(sin, -pi, pi * 3)
plot(smdt$stuno, smdt$Korean)
plot(smdt$stuno, smdt$Korean, col='#0000FF')   # col='red'
colors()
plot(x = smdt$stuno, y = smdt$Korean,
     col = '#0000FF',
     cex = 3,
     las= 1,
     type = 'l',         # p, l, b, c, o, s
     xlim = c(-0.5, 5.5),
     ylim = c(50, 100),
     pch = 8,                         # > ?points
     xlab = '학번', ylab = '국어',
     main = '그래프 타이틀')

# plot - overlap & legend ####
xl = c(-0.5, 40.5)
yl = c(30, 100)
plot(x = smdt$stuno, y = smdt$Korean,
     col='#0000FF', cex=3, pch = 8,
     xlim = xl, ylim = yl,
     xlab = '학번', ylab = '국어, 수학',
     main = '우리반 국어 / 수학 성적')
par(new = T)
plot(x = smdt$stuno, y = smdt$Math,
     col='#ff0000', cex=3, pch = 21,
     xlim = xl, ylim = yl,
     xlab = '', ylab = '')

legend('bottomright',      # center, top & bottom, left & rigth
       legend=c('국어', '수학'),
       pch=c(8, 21), col=c('blue', 'green'), bg='gray')

# plot - barplot ####
library(dplyr)
t = data %>% filter(eng > 90) %>% select('cls', 'gen') %>% table
barplot(t,
        beside = T,
        border = 'dark blue',
        density = 20*4:1,
        angle = 30*1:4,
        xlab = '학급별 성별', ylab = '영어',
        legend=rownames(t),
        col=heat.colors(4),
        xlim=c(0,15),
        horiz= T,
        cex.names = 2,
        las=1)

# boxplot(데이터변수~기준변수, data=데이터, col=...)
boxplot(kor~cls, data=data, col='lightblue')
boxplot(kor~gen, data=data, col='lightblue')
# hist(데이터, labels=T/F, breaks=개수, col=...)
hist(data$국어, col="gray", labels=T, breaks=10)
# curve(expr, from, to, n, xname, xlab, ylab, xlim, type)
curve(sin, -2 * pi, 3 * pi,
      xname='x', xlab = 'TT',
      n=200, type='p',
      xlim=c(-10,15), ylim=c(0,10))

# plot - pie ####
#pie(x, labels=names(x), edges=200, radius=0.8, clockwise=F, init.angle=if(clockwise) 90 else 0, dentity=NULL, angle=45, col=, border, lty, main)

d = data %>% filter(kor > 90) %>% select('cls')
pie(table(d))
pie(table(d), clockwise = T)
pie(table(d), clockwise = T, col=c('red', 'purple', 'green', 'cyan'))


g1 = ggplot(mpg, aes(displ)) +
  geom_histogram(aes(fill=class), 
                 binwidth = .3,     # 또는  bins = 5
                 col='black',       # line color
                 size=.1) +         # line size
  labs(title = 'Title', subtitle = 'Sub Title')


g2 = ggplot(mpg, aes(manufacturer)) +
  geom_bar(aes(fill=class),
           width = 0.7) +
  theme(axis.text.x = element_text(angle=45,
                                   vjust=0.6)) +
  labs(title = 'Title', subtitle = 'Sub Title')
g3 = ggplot(mpg, aes(cty)) +
  geom_density(aes(fill=factor(cyl)), alpha=0.8) +
  labs(title="밀도그래프", subtitle = "실린더수에 따른 시내연비의 밀도그래프",
       caption="Source: ggplot2::mpg",
       x = "도시 연비",
       fill = "실린더수")

g4 = grid.arrange(g2,g3,ncol=2)
grid.arrange(g1,g4,nrow=2)

# Try This : ggplot2 ####
#1
library(dplyr)
d1 = mpg %>%
  filter(year == 1999) %>%
  group_by(year,displ) %>%
  summarise(m1 = mean(cty), m2 = mean(hwy))
d2 = mpg %>%
  filter(year == 2008) %>%
  group_by(year, displ) %>%
  summarise(m1 = mean(cty), m2 = mean(hwy))
b = bind_cols(d1, d2)
View(b)

ggplot( b, aes(x=displ)) +
  geom_line(aes(y=m1, color='1999 cty')) +
  geom_line(aes(y=m2, color='1999 hwy')) +
  geom_line(aes(y=m11, color='2008 cty'), size=1.3) +
  geom_line(aes(y=m21, color='2008 hwy'), size=1.3) +
  scale_colour_manual("", breaks = c("1999 cty", "1999 hwy","2008 cty", "2008 hwy"),
                      values = c("red", "grey", "green", "purple")) +
  xlab("배기량") +
  xlim(1, 7) +
  scale_y_continuous("연비", limits = c(5, 45)) +
  labs(title = '연도별 통합연비', subtitle = '굵은선 = 2008년')

#2
ttt = data%>%filter(korean>=80)
ggplot(ttt, aes(class)) +
  geom_bar(aes(fill=gender),
           width = 0.5) +
  theme(axis.text.x = element_text(angle=0,       # 글씨의 기울기
                                   vjust=0.6)) +   # 글씨의 하단 맞춤(띄우기)
  scale_fill_discrete(name = "성별") +      # legend
  labs(title = 'Title', subtitle = 'Sub Title')

#3
tttt = data %>% group_by(class) %>% filter(korean >= 95)
ggplot(tttt, aes(korean)) +
  geom_density(aes(fill=factor(class)), alpha=0.8) +
  labs(title="밀도그래프", subtitle = "국어성적에 따른 학급별 밀도그래프",
       caption="Source: ggplot2::mpg",
       x = "성적",
       y = "밀도",
       fill = "학급")

#4 지역별 전체인구와 아시아계 인구의 관계
ttttt = midwest %>% filter(poptotal<=500000, popasian<=10000)
ggplot(ttttt, aes(poptotal,popasian)) +
  geom_jitter() +
  labs(title="전체 인구와 아시아계 인구의 관계",
       x="전체인구",
       y="아시아계 인구")
data
