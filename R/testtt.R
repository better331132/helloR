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

k = ggplot( b, aes(x=displ)) +
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
bar = ggplot(ttt, aes(class)) +
  geom_bar(aes(fill=gender),
           width = 0.5) +
  theme(axis.text.x = element_text(angle=0,       # 글씨의 기울기
                                   vjust=0.6)) +   # 글씨의 하단 맞춤(띄우기)
  scale_fill_discrete(name = "성별") +      # legend
  labs(title = 'Title', subtitle = 'Sub Title')
bar

#3
tttt = data %>% group_by(class) %>% filter(korean >= 95)
bar2 = ggplot(tttt, aes(korean)) +
  geom_density(aes(fill=factor(class)), alpha=0.8) +
  labs(title="밀도그래프", subtitle = "국어성적에 따른 학급별 밀도그래프",
       caption="Source: ggplot2::mpg",
       x = "성적",
       y = "밀도",
       fill = "학급")
bar2

#4 지역별 전체인구와 아시아계 인구의 관계
ttttt = midwest %>% filter(poptotal<=500000, popasian<=10000)
bar3 = ggplot(ttttt, aes(poptotal,popasian)) +
  geom_jitter() +
  labs(title="전체 인구와 아시아계 인구의 관계",
       x="전체인구",
       y="아시아계 인구")
data

# 투시도 - persp() ####
x = 1:5; y = 1:3
z = outer(x, y, function(x,y) { x + y })   # dim(x) * dim(y)
z
# persp(x, y, z, theta=a, phi=a, col='..', expand=...)
x = seq(-10, 10, length=30); y = x
f = function(x, y) {
r = sqrt(x^2 + y^2)
return (10 * sin(r) / r)
}
z = outer(x, y, f)
persp(x, y, z, theta = 0, phi = 0, expand = 0.5, col='lightblue',
      ltheta = 120, shade = 0.75, ticktype='detailed',
      xlab = 'X', ylab = 'Y', zlab = "Sinc(r)")

# 단계구분도 (Choroleth Map)-전처리####
head(USArrests)
str(USArrests) 
rownames(USArrests)
# state 변수가 없이 rownames가 state!!
library(tibble)    # install.packages('dplyr')
chodata = rownames_to_column(USArrests, var = 'state')
chodata$state = tolower(chodata$state)
# other way
chodata = data.frame(state = tolower(rownames(USArrests)), USArrests)
head(chodata)

# 단계구분도(Choroleth Map)-그리기####
#ggiraphExtra::ggChoropleth()   #https://cran.r-project.org/web/packages/ggiraphExtra/index.html
install.packages('ggiraphExtra')
install.packages('maps')
library(ggiraphExtra)
usmap = map_data('state') 
head(usmap)

install.packages('mapproj')
ggChoropleth(data=chodata,
             aes(fill=Murder, map_id=state),
             map = usmap,
             title = '..',
             reverse = F,
             interactive = T)

# customize tooltip & click ####
tooltips = paste0(
  sprintf("<p><strong>%s</strong></p>", as.character(chodata$state)),
  '<table>',
  '  <tr>',
  '    <td>인구(ten thousand)</td>',
  sprintf('<td>%.0f</td>', chodata$UrbanPop * 10),
  '  </tr>',
  '  <tr>',
  '    <td>살인</td>',
  sprintf('<td>%.0f</td>', chodata$Murder),
  '  </tr>',
  '  <tr>',
  '    <td>pokryuk</td>',
  sprintf('<td>%.0f</td>', chodata$Assault),
  '  </tr>',
  '</table>' )
tooltips = stringi::stri_enc_toutf8(tooltips)

onclick = sprintf("alert(\"%s\")", as.character(chodata$state))

install.packages("ggiraph")
#ggplot() + geom_map_interactive(tooltip=.., onclick=..) → ggiraph() → girafe()

library(ggiraph)
ggplot(chodata, aes(data = Murder, map_id = state)) +
  geom_map_interactive( 
    aes(fill = Murder,
        data_id = state,
        tooltip = tooltips,
        onclick = onclick), 
    map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  scale_fill_gradient2('살인', low='red', high = "blue", mid = "green") +
  labs(title="USA Murder") -> gg_map
ggiraph(code = print(gg_map))
girafe(ggobj = gg_map)

Sys.getlocale()

# 우리나라 단계 구분도 -kormaps ####
install.packages('devtools')
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
#kdata = kormaps2014::changeCode(korpop1)
kdata = kdata %>% rename(pop = 총인구_명)
kdata = kdata %>% rename(area = 행정구역별_읍면동)
kdata = korpop1
View(kdata)
ggChoropleth(data=kdata, 
             aes(fill = pop, map_id = code, tooltip = area),
             map = kormap1,
             interactive = T)
#kdata$area = stringi::stri_enc_toutf8(kdata$area)

ggplot(kdata, aes(data = pop, map_id = code)) +
  geom_map( aes(fill = pop), map = kormap1) + 
  expand_limits(x = kormap1$long, y = kormap1$lat) +
  scale_fill_gradient2('인구', low='darkblue') +
  xlab('경도') + ylab('위도') + 
  labs(title="시도별 인구")
kdata

# Interactive Graph - plotly
install.packages('plotly')
library(plotly)
t=ggplot(data, aes(english, korean)) +
  geom_point(aes(color=english, size=korean), alpha=0.3)
colnames(data)
ggplotly(t)
ggplotly(bar)
ggplotly(bar2)
ggplotly(bar3)

install.packages('dygraphs')
library(dygraphs)
library(xts)
economics
ue = xts(economics$unemploy, order.by = economics$date)
dygraph(ue)
dygraph(ue) %>% dyRangeSelector()

psave = xts(economics$psavert, order.by = economics$date)
ue2 = xts(economics$unemploy / 1000, order.by = economics$date)
pu = cbind(ue2, psave)
dygraph(pu) %>% dyRangeSelector()
kormaps2014::tbc

#Try This: choropleth ####
#1 다음과 같이 미국의 범죄율을 한번에 작도하시오.

USA = USArrests
usmap = map_data('state')
ggChoropleth(data=chodata, 
             aes(fill = c(Murder, Assault,UrbanPop,Rape), map_id = state),
             map = usmap,
             interactive = T)

#2미국 범죄율의 Rape부분을 단계 구분도로 작성하시오.(단, 툴팁은 그림과 같이 표현하고, 클릭시 해당 state의 wikipedia 페이지를 보이도록 HTML로 저장하시오)
tooltip_r = paste0(
  sprintf("<p><strong>%s</strong></p>", as.character(chodata$state)),
  '<table>',
  '  <tr>',
  '    <td></td>',
  sprintf('<td>%.0f</td>', chodata$Rape),
  '    <td>/</td>',
  sprintf('<td>%.0f</td>', chodata$UrbanPop * 10),
  '    <td>만</td>',
  '  </tr>',
  '</table>' )
tooltip_r = stringi::stri_enc_toutf8(tooltip_r)

onclick = sprintf("window.open(\"https://en.wikipedia.org/wiki/%s\")", as.character(chodata$state))
ggplot(chodata, aes(data = Rape, map_id = state)) +
  geom_map_interactive( 
    aes(fill = Rape,
        data_id = state,
        tooltip = tooltip_r,
        onclick = onclick), 
    map = usmap) +
  expand_limits(x = usmap$long, y = usmap$lat) +
  scale_fill_gradient2('Rape', low='red', high = "blue", mid = "green") +
  labs(title="US Rape") -> gg_Rape
ggiraph(code = print(gg_Rape))
girafe(ggobj = gg_Rape)

#3시도별 결핵환자수(kormaps::tbc)를 단계 구분도로 작성하시오.(우리나라) (단, 환자수는 2006년부터 2015년 총합, NA인 지역은 0으로 표시할 것)
tbc = kormaps2014::tbc
tbc = kormaps2014::changeCode(tbc)
tbc$NewPts = ifelse(is.na(tbc$NewPts),0,tbc$NewPts)
tbc$year = as.numeric(tbc$year)
tbc$NewPts = as.numeric(tbc$NewPts)
tbcpart = tbc %>% filter(year %in% 2006:2015) %>% group_by(code,name) %>% summarise(SNewPts = sum(NewPts))
tooltip_t = paste0(
  sprintf("<p>%s</p>", as.character(tbcpart$name)),
  '<table>',
  '  <tr>',
  '    <td></td>',
  sprintf('<td>%.0f명</td>', tbcpart$SNewPts),
  '  </tr>',
  '</table>' )
tooltip_t = stringi::stri_enc_toutf8(tooltip_t)

ggChoropleth(data=tbcpart, 
             aes(fill = SNewPts, map_id = code, tooltip = name),
             map = kormap1,
             interactive = T)

ggplot(tbcpart, aes(data = SNewPts, map_id = code)) +
  geom_map_interactive( 
    aes(fill = SNewPts,
        data_id = code,
        tooltip = tooltip_t), 
    map = kormap1) +
  expand_limits(x = kormap1$long, y = kormap1$lat) +
  scale_fill_gradient2('Rape', low='red', high = "blue") +
  labs(title="Korea tbc") -> korea_tbc
ggiraph(code = print(korea_tbc))
girafe(ggobj = korea_tbc)
