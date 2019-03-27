Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_201/")
install.packages(c("rJava", "memoise", "KoNLP"))
library(rJava)
library(KoNLP)


install.packages(c("twitteR", "RCurl", "RJSONIO", "stringr", "streamR", "ROAuth"))
library(twitteR); library(RCurl); library(RJSONIO); library(stringr);library(streamR); library(ROAuth)
library(dplyr)
library(RColorBrewer)
library(wordcloud)

load("data/api_keys.rda")
setup_twitter_oauth(api_keys[1], api_keys[2], api_keys[3], api_keys[4])



tweets = searchTwitter(enc2utf8('심마담'), n=10000, lan='ko', since='2017-01-01', until='2019-03-31')
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

# 텍스트마이닝 기법( 연관성 분석 ) ####
install.packages(c("arules", "igraph", "combinat", "arulesViz", "visNetwork"))
library(arules); library(igraph); library(combinat)
library(arulesViz); library(visNetwork)
nouns = sapply(wc, unique)
nouns
nouns1 = sapply(nouns, function(x) {
  Filter(function(y='') { nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y) }, x)
})
wtrans = as(nouns1, "transactions")
rules = apriori(wtrans, parameter = list(supp=0.05, conf=0.8))
inspect(sort(rules))

# 연관성 시각화 ####
#install.packages(c("arulesViz", "visNetwork"))
subrules2 <- head(sort(rules, by="lift"), 40)
ig <- plot( subrules2, method="graph", control=list(type="items") )
ig_df <- get.data.frame( ig, what = "both" )
visNetwork(
  nodes = data.frame(id = ig_df$vertices$name,
                     value = ig_df$vertices$support, ig_df$vertices),
  edges = ig_df$edges
) %>% visEdges(ig_df$edges) %>%visOptions( highlightNearest = T )


