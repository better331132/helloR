# Try THis : Naver News ####
library(rvest)
library(KoNLP)
library(RColorBrewer)
library(wordcloud)
library(arulesViz)
library(visNetwork)
library(arules); library(igraph); library(combinat)
#1 네이버 뉴스 1면의 기사들을 수집하시오.
newsUrl = "https://news.naver.com/main/home.nhn"
html = read_html(newsUrl)
links = html_attr(html_nodes(html, '.main_component.droppable li a'), 'href')
links = gsub('https://news.naver.com/main/live.*', NA,links)
links = links[!is.na(links)]
lastnum = length(links)
links

naver_news = list()

for (i in 1:lastnum) {
  try({
    htxt = read_html(links[i])
    comments = html_nodes(htxt, '#articleBodyContents')
    get_news = repair_encoding(html_text(comments), from='utf-8')
    naver_news[i] = get_news
  }, silent = F)
}

naver_news
length(naver_news)
#2 수집 된 뉴스로 WordCloud를 작도하시오.
bpstr = function(x){
  x = gsub('ⓒ.*', ' ', x)
  x = gsub('▶.*' ,' ', x)
  x = gsub('▶.*' ,' ', x)
  x = gsub('....기자', ' ', x)
  x = gsub('[[:alnum:]]+@[[:alnum:].]+', ' ', x)
  x = gsub("flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", x)
  x = gsub("[[:cntrl:]]", " ", x)
  x = gsub("http[s]?://[[:alnum:].\\/]+", " ", x)
  x = gsub("&[[:alnum:]]+;", " ", x)
  x = gsub("@[[:alnum:]]+[:]?", " ", x)
  x = gsub("[ㄱ-ㅎㅏ-ㅣ]"," ",x)
  x = gsub('<.*>', ' ', enc2native(x))
  x = gsub("\\s{2,}", " ", x)
  x = gsub("[[:punct:]]", " ", x)
  x = gsub("[[:digit:]]", " ", x)
  x = gsub("\\W"," ", x)
  gsub("\\s{2,}", " ", x)
  
}
lastnum = length(naver_news)
lastnum
bpstr(naver_news[[3]][1])

for (i in 1:lastnum){
  if (is.null(naver_news[[i]][1])) next;
  naver_news[[i]][1] = bpstr(naver_news[[i]][1])
}
naver_news
nnnoun = sapply(naver_news, extractNoun, USE.NAMES = F)
nnnoun1 = table(unlist(nnnoun))

pal = brewer.pal(9, 'Reds')
wordcloud(names(nnnoun1), freq=nnnoun1, scale=c(5,0.5), rot.per=0.25, min.freq = 4 ,random.order = F, random.color = T, colors = pal)

#3 수집 된 뉴스로 연관성분석을 하시오.
nnnoun = sapply(naver_news, extractNoun, USE.NAMES = F)
nnnoun = sapply(nnnoun, unique)
nnnoun
nnnoun2 = sapply(nnnoun, function(x) {
  Filter(function(y='') { nchar(y) <= 7 && nchar(y) > 1 && is.hangul(y) }, x)
})
wtrans = as(nnnoun2, "transactions")
rules = apriori(wtrans, parameter = list(supp=0.076, conf=0.76))
inspect(sort(rules))

subrules3 <- head(sort(rules, by="lift"), 400)
ig3 <- plot( subrules3, method="graph", control=list(type="items") )
ig_df3 <- get.data.frame( ig3, what = "both" )
visNetwork(
  nodes = data.frame(id = ig_df3$vertices$name,
                     value = ig_df3$vertices$support, ig_df3$vertices),
  edges = ig_df3$edges
) %>% visEdges(ig_df3$edges) %>%visOptions( highlightNearest = T )
