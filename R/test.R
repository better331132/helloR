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
