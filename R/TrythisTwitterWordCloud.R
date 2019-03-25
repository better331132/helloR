install.packages(c("twitteR", "RCurl", "RJSONIO", "stringr", "streamR", "ROAuth"))
library(twitteR); library(RCurl); library(RJSONIO); library(stringr);library(streamR); library(ROAuth)
library(dplyr)
library(RColorBrewer)
library(wordcloud)

load("data/api_keys.rda")
setup_twitter_oauth(api_keys[1], api_keys[2], api_keys[3], api_keys[4])



tweets = searchTwitter(enc2utf8('심마담'), n=1000, lan='ko', since='2019-03-01', until='2019-03-31')
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

# 1. noun-extraction
nouns = sapply(tdf, extractNoun, USE.NAMES = F)
wc = sapply(tw, extractNoun, USE.NAMES = F)
wc
wc1 = table(unlist(wc))
wc1
brewer.pal.info
# 2. wordcloud
pal = brewer.pal(9, 'Reds')
wordcloud(names(wc1), freq=wc1, scale=c(5,0.5), rot.per=0.25, min.freq = 4, random.order = F, random.color = T, colors = pal)

library(RCurl)
extractNoun

install.packages(c("rJava", "memoise", "KoNLP"))
library(rJava)
library(KoNLP)
