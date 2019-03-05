# factor, vector, matrix Try this ####
# 1
blood = function(n){
  print(factor(n,levels=1:4,labels=c('A','B','O','AB')))
}
# 2
apvector=function(v,n){
  v[length(v)+1]=n
}
v1=c(1)
apvector(v1,3)
# 3
m = matrix(1:200, nrow=10, ncol=20, byrow=True)
colnames(m)=LETTERS[1:ncol(m)]
rownames(m)=letters[1:nrow(m)]
colnames(m)[20]=paste(LETTERS[20],20,sep='')
colnames(m)[10]=paste(LETTERS[10],10,sep='')
rownames(m)[10]=paste(letters[10],10,sep='')
#dataframe trythis ####
#1
df4 = cbind(df2,df3[,c(4:ncol(df3))])
#2
df4[,c('학번','반','성별','국어','과학','수학','영어','예체')]
data()
dr = data()$result
dr
str(dr)
head(dr)
class(dr)
dr[,'Item']
rm(ap)
data("AirPassengers")
class(AirPassengers)
AirPassengers
data("trees")
trees
class(trees)
letters
LETTERS
month.name
month.abb

# package ####
install.packages('data.table')

library('data.table')

install.packages("ggplot2_3.1.0.zip",repos=NULL,type="source")

# read sep file ####
sepdata = read.delim('data/sep.txt', sep='#')
sepdata
class(sepdata)
str(sepdata)
summary(sepdata)
View(sepdata)

# column class 맞추기 ####
sepdata$name
sepdata$name = as.character(sepdata$name)
class(sepdata$name)
str(sepdata)

# Tab seperated Values(data.frame)####
tsvdata = read.table("data/tcv.tsv", sep='\t')
tsvdata = read.table("data/tcv.tsv", sep='\t', header=TRUE)
tsvdata = read.table("data/tcv.tsv", sep='\t', header=T, stringsAsFactors=F)
str(tsvdata)
View(tsvdata)

# Fixed Width File (data.frame)####
fwfdata = read.fwf('data/fwf.txt', header=F, width=c(8,6,5,3,4))#헤더가 있으면 고정폭에 문제가 생길수도있음
fwfdata
str(fwfdata)
dim(fwfdata)
head(fwfdata)
summary(fwfdata)
#install.packages('readxl')####
#library('readxl')####
mtx = read_excel('data/meltop100.xlsx')
mtx = read_excel('data/meltop100.xlsx', sheet = 1)
mtx = read_excel('data/meltop100.xlsx', sheet = 1, col_names = F)
mtx
str(mtx)
dim(mtx)
View(mtx)
#데이터탐색 ####
#install.packages('psych')
#library('psych')
#Trythis 데이터 불러오기, 내보내기####
#1
mt100=read_excel('data/meltop100.xlsx', sheet = 1)
mt100
mtexrow = mt100[-nrow(mt100),]
save(mtexrow, file='data/mtexrow.rda')

#2
meltop100 = read.csv('data/meltop100.csv')
View(meltop100)

#3
temper = read.delim('data/temper.txt',header = F)
year = substr(temper[,1],16,19)
tpr = substr(temper[,1],88,92)
del = substr(temper[,1],93,93)
tprtable = cbind(year,tpr,del)
tprdata = as.data.frame(tprtable)
tprdata

#3-1
temper_pro = read.fwf('data/temper.txt', header=F, width=c(15, 4, 68, 5, 1))


data[data$'학번'==10337,]
km = data[data$'국어'>90 & data$'수학'>90]
km[order(km$수학),]

#Trythis 데이터탐색 ####
#1
fdata=data[data$반 =='난' & data$성별=='남' & data$국어 > 90 & data$수학>90,]
fdata
#2
data$평균 = (data[,4] + data[,5] + data[,6] + data[,7] + data[,8])/5
kcmean = aggregate(data=kdata,국어~반,mean)
ktmean = mean(kdata$국어)
kcmean
ktmean

#If Else ####
install.packages("ggplot2")
library(ggplot2)
data$scout = ifelse(data$평균>=60, ifelse(data$성별=='남','BoyScout','GirlScout'),'')
data
qplot(data$pass)
qplot(data$scout)

#TryThis ggplot2 ####
#1
qplot(data$scout)
qpdata = data[data$평균>=60,]
qplot(qpdata$scout)

#2
data$grade=ifelse(data$평균>=90,'A',ifelse(data$평균>=80,'B',ifelse(data$평균>=70,'C',ifelse(data$평균>=60,'D','F'))))
data

#3
qplot(data$grade)


data$test = ifelse(data$평균 >= 90, 'A','B')
data[data$test =='B',]$test = ifelse(data$평균 >=80,'B','C')

#TryThis portfolio ####
#1
mpg$meanch = (mpg$cty+mpg$hwy)/2
ordermpg = order(-mpg$meanch)
ordermpg

#2
rmpg=mpg[,c(4,12,10)]
grmpg = aggregate(data=rmpg,meanch~year+fl,mean)
ogrmpg = grmpg[order(grmpg$year),]
ogrmpg

#3
midwest = as.data.frame(ggplot2::midwest)
st = aggregate(data=midwest, total~state, sum)
at = aggregate(data=midwest, asian~state, sum)
tableapps = cbind(st,at[,2])
colnames(tableapps)[3]='asian'
tableapps
#4
colnames(midwest)[5]='total'
colnames(midwest)[10]='asian'
#5
ta = sum(midwest$asian)
midwest$asianpct = midwest$asian / ta
hist(midwest$asianpct)
#6
apps = aggregate(data=midwest, asianpct~state, sum)
barplot(apps$asianpct, names.arg=apps$state, main="주별 아시아인 인구분포")
#7
apavg = mean(midwest$asianpct)
midwest$asianrate = ifelse(midwest$asianpct > apavg,'lg','sm')
#8
qplot(midwest$asianrate)

# rep ####
rep(1, times=3)
rep(LETTERS[1:5], times=4)
rep(LETTERS[1:5], each=4)
rep(c('I', 'am'), times=2, length.out=7)
# seq ####
-3:5
5.5:-4.5
seq(1,10,by=2)
seq(5.4, -4.5)
seq(5.4, -4.5, length.out=10)
seq(from=1, to=88, by=3)
#runif ####
runif(20)
t=runif(n=30, min=10, max=20)
t
#sample ####
plot(t[order(t)])
sample(1:5, size=3)
set.seed(100);sample(1:5,size=3)
sample(1:50, size=30, replace=F)
data[sample(1:nrow(data), size=5,replace = F),]
data$c1 = sample(c('AA','BB'), size=nrow(data), replace=T)
cat(nrow(data),nrow(data[data$c1 == 'AA',]))
data$c1 = sample(c('AA','BB'), size=nrow(data), replace=T, prob=c(0.5,0.5))
data
cat(nrow(data),nrow(data[data$c1 == 'AA',]))
#set.seed ####
set.seed(255); sample(1:100,10)
smdt = data.frame(stuno=1:30,
                   Korean=sample(10:50,30)+sample(10:50,30),
                   English=sample(10:50,30)+sample(10:50,30),
                   Math=sample(10:50,30)+sample(10:50,30))

library(psych)
describe(smdt2)
#문자열 함수 ####
s = "abc,efg,abc"
nchar(s)
substr(s, 1, 5)
strsplit(s, ',')
sub('abc', 'ttt', s)
gsub('abc', 'ttt', s)
cat(s)
print(s)
paste('aaa-bbb', 'ccc-ddd', sep='**')
paste0('aaa-bbb', 'ccc-ddd')
outer(month.abb, 2011:2020, paste, sep='-')
outer(LETTERS, 2010:2020, paste0)
grep(pattern='^3', x=data$학번, value = T) #value= T(값을 출력, F: 행)
grep(pattern='^2.*0$', x=data$학번, value = T)# 2로시작, 중간에 아무거나, 끝에 0
s="aBc,efg,abc"
gsub(pattern='[,]+a(.*)',replacement = 'ttt', x=s, ignore.case=T)
grep(pattern='[,]+a(.*)', x=s,value=T)

# 시간함수 ####
as.Date('2019-03-04 09:00')
dt1 = as.POSIXct('2019-03-04 09:00')
seq(dt1, as.POSIXct('2019-04-01'),by='day')
seq(dt1, as.POSIXct('2019-04-01'), by='2 hour')
seq(dt1, as.POSIXct('2019-04-01 23:59'), by='min')
#install.packages('lubridate')
#library(lubridate)
ymd('20190305')
mdy('03052019')
year(dt1)
day(dt1)=15
dt1
days_in_month(1:12)
ddays(10)
dhours(50)
duration(1000)
round(as.POSIXct('2019-03-05 18:39:45'), 'month')#min, hour, day, month, year단위까지 반올림 가능
#if ####

#switch ####
switch(2, "111","222","333")
switch(1, c("111","222","333"))

#loop ####
#for loop
for(i in 1:3){print(i)}
for(r in 1:nrow(data)){print(data[r,'scout'])}
#while loop
i=0
while(i<10){print(i); i= i+1}
#break & next(continue)
i=0
while(TRUE){
  i = i + 1
  if (i %% 2)
    next
  if ( i > 10 )
    print(i)
    break
}
source('factorial.R')
# '<-' vs '<<-' ####

#apply ####
apply(smdt3[, 2:4], MARGIN = 1, FUN = mean)
apply(smdt3[, 2:4], MARGIN = 2, FUN = mean)
apply(smdt3[, 2:4], MARGIN = 2, FUN = quantile)
#lappy ####
lapply(smdt3[, 2:4], FUN = mean)
unlist(lapply(smdt3[, 2:4], FUN = mean))
#sapply####
sapply(smdt3[, 2:4], FUN = mean, simplify = T)
sapply(smdt3[, 2:4], FUN = mean, simplify = F)
#vapply####
vapply(smdt3[, 2:4], FUN = mean, FUN.VALUE = 1)#FUN.VALUE는 데이터의 타입만 고려함 1이든 10이든 결과에 영향이 없음
#reshape2 - melt, dcast ####
library('reshape2')
dfsum=cbind(data.frame(no=1:4, year=2016:2019),matrix(round(runif(16),3) *1000, ncol=4,dimnames = list(NULL, paste0('Q', 1:4))))
dfsum
# melt(data, id.vars=<기준컬럼>, variable.name=<키 변수 명>)
# `기준컬럼 - Key - Value` 형태로 구조 변경!!
melt(data=dfsum[,2:6], id.vars = "year")
meltsum = melt(dfsum[,2:6], id.vars = "year", variable.name = 'Sales')
meltsum
# dcast(data, <기준컬럼>~<나열컬럼>, value.var=<키 변수 명>)
dcast(meltsum, Sales~year, value.var="value")
#array ####
dataArray = array(1:24, dim=c(3, 4, 2))    # (3X4 matrix) * 2
dataArray
#TryThis -if,loop,while,source,<-vs<<-, apply, reshape2(melt,dcast)####
#1 data$group 컬럼에 A조~C조 랜덤으로 160명씩 고르게 분포시키시오.
data$group = 'A조'
data[data$group=='A조',][sample(1:320,size=320),]$group = 'B조'
data[data$group=='B조',][sample(1:160,size=160),]$group = 'C조'
cat(nrow(data[data$group=='A조',]))
cat(nrow(data[data$group=='B조',]))
cat(nrow(data[data$group=='C조',]))

#2 fibonacci.R 파일을 작성하고 console에서 실행하시오.
while(TRUE) {
  x = as.integer(readline(prompt = "Input the number: "))
  if (x <= 0) break
  
  if (x == 1) 
    print("1")
  else if (x == 2) 
    print("1, 1")
  else{
    output = "1, 1"
    b1 <- 1
    b2 <- 1
    for (i in 3:x){
      b3 <- b1+b2
      output <- paste(output,b3,sep=", ")
      b1 <- b2
      b2 <- b3
    }
    print(output)
  }
}
source('fibonacci.R')

#3 apply를 이용하여 smdt에 과목별 (총)평균점수 행을 추가하고, 총점과 평균 변수(컬럼)을 추가하시오.
smdt[nrow(smdt)+1,2:4] = apply(smdt[,2:4], MARGIN = 2, FUN = mean)
smdt[nrow(smdt),1]="계"
smdt$total = apply(smdt[,2:4],MARGIN = 1, FUN = sum)
smdt$avg = apply(smdt[,2:4],MARGIN = 1, FUN = mean)
smdt

#4 2016~2019년 연도별 1월(Jan) ~ 12월(Dec) 매출액 데이터를 `no year Jan Feb … Dec` 형태로 만든 다음, 아래와 같이 출력하시오.
library('reshape2')
smdt2 = cbind(data.frame(no=1:4, year=2016:2019), matrix(round(runif(48),2)*100, ncol=12, dimnames = list(NULL, month.abb)))
smdt2
melt(data=smdt2[,2:14], id.vars = "year")
fmelt = melt(data=smdt2[,2:14], id.vars = "year", variable.name="Sales")
fmelt
