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
