#1
mpg$meanch = (mpg$cty+mpg$hwy)/2
ordermpg = mpg[order(-mpg$meanch),]
ordermpg

#2
rmpg=mpg[,c(4,12,10)]
grmpg = aggregate(data=rmpg,meanch~year+fl,mean)
ogrmpg = grmpg[order(grmpg$year),]
ogrmpg