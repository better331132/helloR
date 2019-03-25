Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_201/")
install.packages(c("rJava", "memoise", "KoNLP"))
library(rJava)
library(KoNLP)


install.packages(c("twitteR", "RCurl", "RJSONIO", "stringr", "streamR", "ROAuth"))
api_key = "rVIPG3JfNPj3pekNcyM1JjSDz"
api_secret = "1OrjfJq6Qe3WAdCOUvdmXe4ILb6z45eYn5XdlUbfrwypWksppQ"
token = "1108927737190768641-ZjkB0DcgSTt5tI3nIoIlITNCeOu2jD"
token_secret = "IY15gghpjHeAkhhL2Ai6sMLB6xZMU4KOlb7lWIrBXbgvz"
api_keys = c(api_key, api_secret, token, token_secret)
setup_twitter_oauth(api_key, api_secret, token, token_secret)
save(api_keys, file = "data/api_keys.rda")


library(twitteR); library(RCurl); library(RJSONIO); library(stringr);library(streamR); library(ROAuth)

searchTwitter(enc2utf8('쪼다'), n=100, lan='ko')

tweets = searchTwitter(enc2utf8('심마담'), n=1000, lan='ko', since='2019-02-01', until='2019-03-31')
tdf = twListToDF(tweets)

# tdf %>% filter(!isRetweet) %>% filter(favoriteCount > 2) # 없네
tdf = tdf %>% filter(regexpr('광고',text) == -1)

tw = unique(tdf$text)

tw = gsub("[[:cntrl:]]", "", tw)                      # 제어문자(\n, \t등) 제거
tw = gsub("http[s]?://[[:alnum:].\\/]+", "", tw)     # link 제거
tw = gsub("&[[:alnum:]]+;", "", tw)            # escape(&amp; &lt;등) 제거
tw = gsub("@[[:alnum:]]+[:]?", "", tw)             # 트위터 계정 부분 제거
tw = gsub("[ㄱ-ㅎㅏ-ㅣ]","",tw)                   # 한글 불용어(ㅋㅋㅎㅎ ㅠㅜ등) 제거
tw = gsub('<.*>', '', enc2native(tw))
tw = gsub("\\s{2,}", " ", tw)                  # 2개이상 공백을 한개의 공백으로 처리
tw = gsub("[[:punct:]]", "", tw)               # 특수 문자 제거 (앞의 처리 때문에 마지막에 해야 함!!)
tw = gsub("RT", "", tw)

tw

# 1. 명사 추출
nouns = sapply(tdf, extractNoun, USE.NAMES = F)
wc = sapply(tw, extractNoun, USE.NAMES = F)
wc1 = table(unlist(wc))
names(wc1)
length(wc1)
# 2. 상위 n 개 추출
wc2 = head(sort(wc1, decreasing = T), 100)
wc2

# 3. wordcloud
library(RColorBrewer)
library(wordcloud)
pal = brewer.pal(9, "Set1")
wordcloud(names(wc1), freq=wc1, scale=c(5,0.5), rot.per=0.25, min.freq = 4, random.order = F, random.color = F, colors = pal)
