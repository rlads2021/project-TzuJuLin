library(stringr)
library(jiebaR)
library(tmcn)
library(tidyverse)
library(rvest)

#設定PTT網址
ptt.url <- "https://www.ptt.cc"
gossiping.url <- str_c(ptt.url, "/bbs/Gossiping")
gossiping.url
gossiping.session <- html_session(url = gossiping.url)
gossiping.session

#填寫認證表單
gossiping.form <- gossiping.session %>%
  html_node("form") %>%
  html_form()
gossiping.form
gossiping <- submit_form(
  session = gossiping.session,
  form = gossiping.form,
  submit = "yes"
)
gossiping

#擷取特定關鍵字連結
page_number512<-35743                 #5/12
page_number501<-34680                 #5/01
links.article <- NULL

index <- sapply(page_number512:page_number501,function(x){
  str_c(gossiping.url, "/index", x, ".html")
})

links.article<-sapply(index, function(x){
  id<- gossiping %>%
    session_jump_to(x) %>%
    html_nodes("a") %>%
    html_text() %>% 
    str_detect("疫苗")     #關鍵字在此
  lk<-gossiping %>%
    session_jump_to(x) %>%
    html_nodes("a") %>%
    html_attr("href")
  return(lk[id]%>%str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html"))
})


article<-NULL
for(i in seq_along(links.article)){
  if(length(links.article)>0){
    article<-c(article,links.article[[i]])
  }
}
articlelink<-str_c(ptt.url,article)
write.csv(articlelink, file = "articlelink.csv")

#進入連結爬文
push.table <- tibble() # 建⽴推⽂儲存空間
article.table <- tibble() # 建⽴⽂章儲存空間
for (temp.link in articlelink) {
  
  article.url <- temp.link # ⽂章網址
  temp.html <- gossiping %>% jump_to(article.url) # 連結⾄⽂章網址
  article.header <- temp.html %>%
    html_nodes("span.article-meta-value") %>% # 開頭部分元素
    html_text()
  article.author <- article.header[1] %>% str_extract("^[A-z0-9_]+") # 作者
  article.title <- article.header[3] # 標題
  article.datetime <- article.header[4] # 時間
  article.content <- temp.html %>%
    html_nodes( # 內⽂部分
      xpath = '//div[@id="main-content"]/node()[not(self::div|self::span[@class="f2"])]'
    ) %>%
    html_text(trim = TRUE) %>%
    str_c(collapse = "")
  article.table <- article.table %>% # 合併⽂章資料
    bind_rows(
      tibble(
        datetime = article.datetime,
        title = article.title,
        author = article.author,
        content = article.content,
        url = article.url
      )
    )
  
  article.push <- temp.html %>% html_nodes("div.push") # 擷取推⽂
  push.table.tag <- article.push %>% html_nodes("span.push-tag") %>% html_text(trim =
                                                                                 TRUE) # 推⽂種類
  push.table.author <- article.push %>% html_nodes("span.push-userid") %>% html_text(trim
                                                                                     = TRUE) # 作者
  push.table.content <- article.push %>% html_nodes("span.push-content") %>%
    html_text(trim = TRUE) %>% str_sub(3) # 推⽂內容
  push.table.datetime <- article.push %>% html_nodes("span.push-ipdatetime") %>%
    html_text(trim = TRUE) # 推聞時間
  
  push.table <- push.table %>% # 合併推⽂資料
    bind_rows(
      tibble(
        tag = push.table.tag,
        author = push.table.author,
        content = push.table.content,
        datetime = push.table.datetime,
        url = article.url
      )
    )
  
}
article.table <- article.table %>% # 格式整理清除 NA
  mutate(
    datetime = str_sub(datetime, 5) %>% parse_datetime("%b %d %H:%M:%S %Y"),
    month = format(datetime, "%m"),
    day = format(datetime, "%d")
  ) %>%
  filter_all(
    all_vars(!is.na(.))
  )
push.table <- push.table %>% # 格式整理清除 NA
  mutate(
    datetime = str_c("2021/", datetime) %>% parse_datetime("%Y/%m/%d %H:%M"),
    month = format(datetime, "%m"),
    day = format(datetime, "%d")
  ) %>%
  filter_all(
    all_vars(!is.na(.))
  )
        
write.csv(article.table, file = "article_table.csv")
write.csv(push.table, file = "push_table.csv")

#斷詞by  jiebaR

library(jiebaR)
library(readr)
jieba.worker <- worker(user = "~/code/R/dict2.txt")
stop2 <- read_csv("code/R/stop2.txt")

article.date <- article.table %>%
  group_by(day) %>% # 以每⽇做分組
  do((function(input) {
    freq(segment(input$content, jieba.worker)) %>% # 斷詞後計算詞頻
      filter(
        !(char %in% toTrad(stop2[[1]])), # 過濾 stopword
        !str_detect(char, "[A-z0-9]"), # 過濾英⽂數字
        nchar(char) > 1 # 過濾單個字
      ) %>%
      arrange(desc(freq)) %>% # 以詞頻排序
      #slice(1:100) %>% # 取前 100
      return
  })(.)) %>%
  ungroup
article.date.words <- freq(article.date$char) %>%
  rename(freq.all = freq)
article.everyday <- article.date %>%
  left_join( # ⽐對全部詞
    article.date.words,
    by = 'char'
  ) %>%
  group_by(day) %>% # 以每⽇做分組
  arrange(freq.all) %>% # 每組的詞頻做排序由⼩到⼤
  slice(1:5) %>% # 取每組前 5
  summarise( # 合併詞並對詞頻加總
    char = str_c(char, collapse = ", "),
    freq = sum(freq)
  ) %>%
  ungroup
write.csv(article.date.words, file = "article_date_words.csv")
write.csv(article.everyday, file = "article_everyday.csv")


#正負面分析
library(dplyr)
library(stringr)
library(tidytext)
library(wordcloud2)
library(data.table)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(tidyr)
library(readr)
library(scales)

#匯入中文情緒辭典
ch_senti_lex <- read_csv("code/R/ch.senti.lex.csv")

P<-ch_senti_lex%>%
  filter(Polarity=="P")%>%
  select(lemma)
N<-ch_senti_lex%>%
  filter(Polarity=="N")%>%
  select(lemma)
P = data.frame(word = P, sentiment = "positive")
N = data.frame(word = N, sentiment = "negative")
LIWC = rbind(P, N)
colnames(LIWC)<-c("char","sentiment")
head(LIWC)

article.date$day<-str_c("5/",article.date$day)
article.date$day<-as.Date(article.date$day,format = "%m/%d")
#5/01-5/12所有詞彙正負面
data<-article.date%>%
  inner_join(LIWC)
colnames(data)<-c("day","word","count","sentiment")

#每日確診人數
covid<-read_csv("code/R/確診人數.csv")
covid$day<-as.Date(covid$day,format = "%m/%d")
covid%>%
  ggplot() +
  geom_bar(mapping = aes(x = day, y = count), stat = "identity",fill="#FF6666")+
  scale_x_date(labels = date_format("%m/%d")) +
  geom_text(aes(x = day, y = count,label=count),size=3,check_overlap = T)
colnames(covid)<-c("day","countsforday")  

#5/01-5/12分日期所有詞彙正負面

sentiment_count <- article.date %>%
  inner_join(LIWC) %>% 
  group_by(day,sentiment) %>%
  summarise(count=sum(freq))
sentiment_count$day<-as.Date(sentiment_count$day,format = "%m/%d")
sentiment_count <- sentiment_count %>%
  inner_join(covid) 
#正負面詞彙每日累計
sentiment_count <-sentiment_count%>%
  inner_join(covid)
sentiment_count %>%
  ggplot()+
  geom_point(aes(x=day,y=count,colour=sentiment))+
  geom_line(aes(x=day,y=count,color=sentiment,group = sentiment))+
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) 
#最常見詞彙詞雲
data %>% 
  select(word,count) %>% 
  group_by(word) %>% 
  summarise(count = sum(count))  %>%
  filter(count>90) %>%   # 過濾出現太少次的字
  wordcloud2()
#正負面詞彙貢獻
data %>%
  group_by(word,sentiment) %>%
  summarise( count = n() )%>% 
  data.frame() %>% 
  top_n(20,wt = count) %>%
  ungroup() %>% 
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(word, count, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  theme(text=element_text(size=14, family = "Heiti TC Light"))+
  coord_flip()
#最常見詞彙詞雲5-3
data %>% 
  filter(day==as.Date("2021/5/3"))%>%
  select(word,count) %>% 
  group_by(word) %>% 
  summarise(count = sum(count))  %>%
  filter(count>15) %>%   # 過濾出現太少次的字
  wordcloud2()

#正負面詞彙貢獻5-3
data %>%
  filter(day==as.Date("2021/5/3"))%>%
  group_by(word,sentiment) %>%
  summarise( count = n() )%>% 
 # arrange(desc(count))
  data.frame() %>% 
  top_n(10,wt = count) %>%
  head(20)%>%
  ungroup() %>% 
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(word, count, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  theme(text=element_text(size=14, family = "Heiti TC Light"))+
  coord_flip()
setwd("~/code/R/")
ggsave("contributed barHD5_3.png",width = 15,height = 15,units = "cm",dpi = 800)

#collocation
library(tidyverse)
library(quanteda)
library(tidytext)
library(tm)
library(DT)

View(data)
str_text<-article.table%>%
#  filter(day=="03")%>%
  select(content)%>%
  lapply(str_c)%>%
  unlist()

text_corpus <- str_text %>% 
  corpus 
summary(text_corpus)
write.csv(kwic(text_corpus, pattern = "保護"), file = "保護.csv")
kwic(text_corpus, pattern = "保護")
#playground ----------------------------------------------------------------------------
 
page_number512<-35743                 #512
page_number506<-34680                 #501
links.article <- NULL
page.length <- page_number512-page_number506
for (page.index in page_number512:page_number506) {
  link <- str_c(gossiping.url, "/index", page.index, ".html")
  #print(link)
  #找關鍵字文章
  id<- gossiping %>%
    session_jump_to( str_c(gossiping.url, "/index", page.index, ".html")) %>%
    html_nodes("a") %>%
    html_text() %>% 
    str_detect("疫苗")     #關鍵字在此
  lk<-gossiping %>%
    session_jump_to( str_c(gossiping.url, "/index", page.index, ".html")) %>%
    html_nodes("a") %>%
    html_attr("href")
  
  
  #製作連結
  links.article <- c(
    links.article,
    lk[id]%>%str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html")
  )
}

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

  id<- gossiping %>%
     session_jump_to( str_c(gossiping.url, "/index", 35743, ".html")) %>%
     html_nodes("a") %>%
     html_text() %>% 
     str_detect("[問卦]")
   
   
   lk<-gossiping %>%
   
     session_jump_to( str_c(gossiping.url, "/index", 35743, ".html")) %>%
     html_nodes("a") %>%
  html_attr("href")
  lk[id]%>%str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html")
#  \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  
  
  
  article.url <- articlelink[1] # ⽂章網址
  temp.html <- gossiping %>% jump_to(article.url) # 連結⾄⽂章網址
  article.header <- temp.html %>%
    html_nodes("span.article-meta-value") %>% # 開頭部分元素
    html_text()
  article.author <- article.header[1] %>% str_extract("^[A-z0-9_]+") # 作者
  article.title <- article.header[3] # 標題
  article.datetime <- article.header[4] # 時間
  article.content <- temp.html %>%
    html_nodes( # 內⽂部分
      xpath = '//div[@id="main-content"]/node()[not(self::div|self::span[@class="f2"])]'
    ) %>%
    html_text(trim = TRUE) %>%
    str_c(collapse = "")
  article.table <- article.table %>% # 合併⽂章資料
    bind_rows(
      tibble(
        datetime = article.datetime,
        title = article.title,
        author = article.author,
        content = article.content,
        url = article.url
      )
    )
  
  article.push <- temp.html %>% html_nodes("div.push") # 擷取推⽂
  push.table.tag <- article.push %>% html_nodes("span.push-tag") %>% html_text(trim =
                                                                                 TRUE) # 推⽂種類
  push.table.author <- article.push %>% html_nodes("span.push-userid") %>% html_text(trim
                                                                                     = TRUE) # 作者
  push.table.content <- article.push %>% html_nodes("span.push-content") %>%
    html_text(trim = TRUE) %>% str_sub(3) # 推⽂內容
  push.table.datetime <- article.push %>% html_nodes("span.push-ipdatetime") %>%
    html_text(trim = TRUE) # 推聞時間
  
  push.table <- push.table %>% # 合併推⽂資料
    bind_rows(
      tibble(
        tag = push.table.tag,
        author = push.table.author,
        content = push.table.content,
        datetime = push.table.datetime,
        url = article.url
      )
    )
  #-----------------------------------------------------------------------------------------
