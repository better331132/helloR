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
melt(data=smdt2[,2:14], id.vars = "year")
fmelt = melt(data=smdt2[,2:14], id.vars = "year", variable.name="Sales")
fmelt