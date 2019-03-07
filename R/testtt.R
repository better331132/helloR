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
