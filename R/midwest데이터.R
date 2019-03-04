#3
#일리노이주와 미시건주 오하이오주의 전체 인구는 크게 차이가 없는 반면 일리노이 주의 아시아인 수가 월등히 높다.
#mean(midwest$percollege)== 18.27274
#mean(midwest[midwest$percasian>1,]$percollege) == 29.87688
#mean(midwest[midwest$percasian>2,]$percollege) == 35.01751
#mean(midwest[midwest$percasian>3,]$percollege) == 38.84174
#mean(midwest[midwest$percasian>4,]$percollege) == 44.04773
#정확한 상관관계 파악은 어려우나 특정 지역의 asian 인구비율이 높을수록 대학진학률이 높아진다는 것을 확인할 수 있다.
midwest = as.data.frame(ggplot2::midwest)
st = aggregate(data=midwest, poptotal~state, sum)
at = aggregate(data=midwest, popasian~state, sum)
tableapps = cbind(st,at[,2])
colnames(tableapps)[3]='asian'
tableapps
hist(tableapps$asian)


#4
colnames(midwest)[5]='total'
colnames(midwest)[10]='asian'
#5
ta = sum(midwest$asian)
midwest$asianpct = midwest$asian / ta
hist(midwest$asianpct)
#6
apps = aggregate(data=midwest, asian~state, sum)
barplot(apps$asian, names.arg=apps$state, main="주별 아시아인 인구분포")
Illinois = midwest[midwest$state=='IL',]
barplot(Illinois$asian, names.arg=Illinois$county, main="일리노이주의 카운티별 아시아인 인구분포")

#7
apavg = mean(midwest$asianpct)
midwest$asianrate = ifelse(midwest$asianpct > apavg,'lg','sm')
#8
qplot(midwest$asianrate)