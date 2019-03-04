#3
#일리노이주와 미시건주 오하이오주의 전체 인구는 크게 차이가 없는 반면 일리노이 주의 아시아인 수가 월등히 높다.
midwest = as.data.frame(ggplot2::midwest)
st = aggregate(data=midwest, poptotal~state, sum)
at = aggregate(data=midwest, popasian~state, sum)
tableapps = cbind(st,at[,2])
colnames(tableapps)[3]='asian'
tableappsta
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
#7
apavg = mean(midwest$asianpct)
midwest$asianrate = ifelse(midwest$asianpct > apavg,'lg','sm')
#8
qplot(midwest$asianrate)