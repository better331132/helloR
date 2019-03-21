print("가가가")
conn
dbListTables(conn)

install.packages('RMySQL')
library(RMySQL)
# load Driver & connection
drv = dbDriver("MySQL")
conn = dbConnect(drv, host='127.0.0.1', port=3306, dbname='betterdb', user='better', password='1q2w3e')
dbSendQuery(conn, 'set character set utf8')   # set utf-8
dbSendQuery(conn, 'set names UTF8;')
dbListTables(conn)       # table list

Sys.getlocale()
Sys.setlocale('LC_ALL', 'en_US.UTF-8')
# SQL → 윈도우에서 한글 깨질 경우 rsdf = changeCode(rsdf)    # library(kormaps2014)


songname = stringi::stri_enc_toutf8("\"빨간빛\"")
changeCode(dbGetQuery(conn, "select * from Melontop limit 5"))
rsdf = dbGetQuery(conn, "select * from Melontop limit 5")
dbGetQuery(conn, paste0("update Melontop set songname=",songname," where id = '1'"))

# Transaction
dbBegin(conn);
dbCommit(conn)
dbRollback(conn) 
# disconnection & unload Driver
dbDisconnect(conn); dbUnloadDriver(drv)

# Trythis:RMySQL ####
library(sqldf)
conmel = dbConnect(drv, host='34.85.73.154', port=3306, dbname='melondb', user='root', password='1q2w3e')
song = dbGetQuery(conmel, "select songNo, genre from Song")
songrank = dbGetQuery(conmel, "select songNo, rank from SongRank")
song
songrank
dbDisconnect(conmel)
detach("package:RMySQL", unload=TRUE)
genrerank = sqldf("select song.songNo, song.genre, songrank.rank from song inner join songrank on song.songNo = songrank.songNo")
class(song)
class(songrank)

ggplot() +
  geom_point(data=genrerank,
             aes(x=rank, y=genre ),
             color='blue', size=2)

genrerank$score = 100/(genrerank$rank+100)
gr = genrerank %>% group_by(genre) %>% summarise(genrescore = sum(score))

ggplot() +
  geom_point(data=gr,
             aes(x=genre, y=genrescore ),
             color='blue', size=2)


library(ggplot2)
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

mpg_select <- mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ]

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")

g + geom_jitter(aes(col=manufacturer, size=hwy)) + 
  geom_smooth(aes(col=manufacturer), method="lm", se=F)

# Text Mining ####
# 정의: `뉴스 기사, SNS 글 등 자연어에서 의미 있는 정보를 찾는 것`
# Web Data Mining, Multimedia Data Mining, Opinion Mining
# Document → Corpus → TermDocument Matrix → Analysis
# 설치
install.packages('tm')
library(tm)
getSources()
getReaders()

# Corpus ####
# 데이터의 정제, 통합, 선택, 변환 과정을 거친 구조화된 문서
# 말뭉치, 말모둠 (연구 대상의 언어 현실을 총체적으로 보여 주는 자료의 집합)
# VCorpus: mem, PCorpus: db or file
VCorpus(x, readerControl = list(reader=reader(x), language='en'))
PCorpus(x, readerControl = list(reader=reader(x), language='en'),
          dbControl=list(dbName="", dbType="DB1"))
# Corpus 생성
folder = system.file("texts", "txt", package="tm")
txtSource = DirSource(folder)   # dir 경로로 Corpus 생성
class(txtSource); str(txtSource)   
doc = VCorpus(txtSource, readerControl = list(language='lat'))
class(doc); summary(doc)

# Corpus 조회 및 저장 ####
# Corpus 정보조회
meta(doc)
meta(doc, type = 'local')
inspect(doc)
inspect(doc[1])
doc[[1]]

# Corpus 객체 저장 (일괄-전체)
writeCorpus(doc, path="data", filenames = names(doc))

# Corpus 객체 저장 (1개)
writeCorpus(doc[1], path="data", filenames = "corpus_doc.txt")

# Corpus 전처리(tm_map)####
#tm_map(doc,FUN,..)
getTransformations()
doc = tm_map(doc, stripWhitespace)

# example: crude(원유 관련 문서)
install.packages('SnowballC')
data("crude")
crude[[1]]
crude[[1]][1]
crude = tm_map(crude, stripWhitespace)
crude = tm_map(crude, content_transformer(tolower))
crude = tm_map(crude, removePunctuation)
crude = tm_map(crude, removeWords, stopwords("english"))
crude = tm_map(crude, stripWhitespace)
crude = tm_map(crude, stemDocument, language="english")
# 특정 단어 제거
crude = tm_map(crude, removeWords, c("xxx", "yyy"))

# 분석용 Term & Document Matrix ####
# 분석용 Matrix 생성 (TermDocumentMatrix & DocumentTermMatrix)
tdm = TermDocumentMatrix(crude)   # 단어(행) * 문서(열)
tdm
colnames(tdm)
rownames(tdm)      # 단어 목록
tail(as.matrix(tdm))        # cf. head(tdm)
tdm['year',]                # cf. tdm['the',]
View(as.matrix(tdm))        # cf. View(tdm)
dimnames(tdm)               # cf. tdm$dimnames
tdm$i                       # 단어 index
tdm$j                       # 문서 index
tdm$v                       # 문서에서 단어의 빈도(단어가 있는 entry별 단어 사용 수) 
tdm = removeSparseTerms(tdm, 0.8)  # 희소성(재사용안됨)이 80% 이상인 단어 제거
tdm; rownames(tdm)
dtm = t(tdm)                # 전치행렬(Transpose)
inspect(tdm)                # 단어의 문서별 사용 수
inspect(tdm[1:5, 1:10])     # 첫 5개 단어 10개 문서 (한번에 최대 10개까지 가능)

# 빈도분석 ####
findFreqTerms(tdm, 20)   # 20회 이상 사용된 단어
findFreqTerms(tdm, 20, 30)    # 20 ~ 30 회 사용된 단어 
findFreqTerms(tdm, 0, 10)     # 10회 이하 사용 단어
findAssocs(tdm, "last", 0.5)       # `last`와 연관성(같이 사용) 50% 이상
findAssocs(tdm, "oil", .7)         # `oil`과 연관성(함께 사용) 70% 이상

# rowSums()  단어별 빈도수 계산
rowSums(as.matrix(tdm))     
wFreq = sort(rowSums(as.matrix(tdm)), decreasing = T)     # 빈도수 높은 순
wFreq
wFreq > 10              

# subset() 특정 조건으로 잘라내기
wFreq = subset(wFreq, wFreq > 10)   # 빈도수 10회 초과 단어들만!
wFreq

# Word Cloud ####
# RColorBrewer - 팔레트
install.packages('RColorBrewer')  
library(RColorBrewer)
display.brewer.all()            # brewer 전체 색상
brewer.pal.info                 # brewer palette 정보
pa = brewer.pal(8, 'Blues')     # Blues Theme에서 8가지 색상 선택
darks = brewer.pal(8, 'Dark2')  # Favorite Palette

# wordcloud - 워드 클라우드
install.packages("wordcloud")     
library(wordcloud)
set.seed(100);
wordcloud(words = names(wFreq), 
          freq=wFreq, 
          min.freq = 1, 
          random.order = F, 
          colors = pa)

# TryThis:wordcloud ####
meta(doc)
meta(doc, type = 'local')
inspect(doc)
inspect(doc[1])
doc[[1]]
names(doc)
writeCorpus(doc, path="data", filenames = names(doc))
writeCorpus(doc[1], path="data", filenames = "corpus_doc.txt")

#getTransformations()

doc[[1]]
doc[1]

# 전처리
doc = tm_map(doc, stripWhitespace)
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, stemDocument, language="english")
doc = tm_map(doc, removeWords, stopwords("english"))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtdm = TermDocumentMatrix(doc)
dtdm
rownames(dtdm)
dtdm = removeSparseTerms(dtdm, 0.8)
inspect(dtdm)
findFreqTerms(dtdm, 2)
rowSums(as.matrix(dtdm))
sdtdm = sort(rowSums(as.matrix(dtdm)), decreasing = T)
names(sdtdm)
sdtdm > 3
sdtdm = subset(sdtdm, sdtdm >=2)
sdtdm
display.brewer.all()
co = brewer.pal(5, 'RdGy')
wordcloud(words = names(sdtdm), freq=sdtdm, min.freq = 1, scale=c(4.5,0.5), rot.per=0.25, random.order = F, random.color = F, colors= co)
