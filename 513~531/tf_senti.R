library(jiebaR)
library(readr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(wordcloud2)


data <- read.csv("./PTT疫苗1.csv")


new_word_list <- scan("./dic.txt" ,character(), quote = "")
writeLines(new_word_list, "./dict2.txt")  
stop_word_list <- scan("./stop.txt" ,character(), quote = "")
writeLines(stop_word_list, "./stop2.txt")  

seg <- worker(user = "./dict2.txt",stop_word = "./stop2.txt")


segment(data$content, seg)

#create a new vector and segment the content
data_segged <- vector("character", length = length(data$content))
for (i in seq_along(data$content)){
  segged <- segment(data$content[i], seg)
  
  # Collapse the character vector into a string, separated by space
  data_segged[i] <- paste0(segged, collapse = "\u3000")
}

#add the segged vector as a new column in data
data$segged <- data_segged

#change the format of the date
date <- vector("character", length = length(data$time))
for (i in 1:24){
  date[i]<- paste(unlist(strsplit(data$time[i], " "))[2], unlist(strsplit(data$time[i], " "))[4])
}
for(i in 25:length(data$time)){
  date[i]<- paste(unlist(strsplit(data$time[i], " "))[2], unlist(strsplit(data$time[i], " "))[3])
}

#add column date into data
data$date <- date


#count word, group by date and word(group_by日期詞頻表)

tidy_text_format <- data %>%
  unnest_tokens(output = "word", input = "segged",
                token = "regex", pattern = "\u3000")%>%
  group_by(date,word)%>%
  summarize(n = n())%>%
  arrange(desc(n))

#wordcloud
tidy_text_format %>%
  #filter(date == "May 24") %>%
  select(word, n)%>%
  group_by(word) %>% 
  summarise(count = sum(n))%>%
  filter(count>2000) %>%
  wordcloud2()

##情緒分析
#read sentiment csv

PN <- read_csv("./ch.senti.lex.csv")

PN_adjusted <- PN %>% 
  rename(sentiment = Polarity) %>%
  rename(word = lemma)%>%
  mutate(sentiment =replace(sentiment, sentiment == "P", "Positive"))%>%
  mutate(sentiment =replace(sentiment, sentiment == "N", "Negative"))%>%
  filter(sentiment %in% c("Negative", "Positive"))%>%
  select(word, sentiment)

#tidy_text_format join with sentiment

word_sentiment <- tidy_text_format %>%
  select(word,date)%>%
  inner_join(PN_adjusted)

#confirmed cases
patient <-data.frame(
  date = c("May 13", "May 14", "May 15", "May 16", "May 17", "May 18", "May 19", "May 20", "May 21", "May 22", "May 23", "May 24", "May 25", "May 26", "May 27", "May 28", "May 29", "May 30", "May 31"),
  cases = c(13, 30, 185, 284, 535, 456, 527, 474, 432, 473, 491, 502, 499, 532, 529, 415, 415, 425, 395)
  
)

#count daily sentiment
daily_sentiment <- word_sentiment %>%
  group_by(date, sentiment) %>%
  summarize(count = n()) %>%
  #爬蟲多餘的文章濾掉
  filter(date %in% c("Jun ", "Jul ", "Jun 1") == FALSE)%>%
  inner_join(patient)


#daily_sentiment_plot (with confirmed cases)
daily_sentiment_plot <- ggplot(daily_sentiment, aes(x = date, y = count, color = sentiment, label = cases)) +  geom_point() + 
  geom_line(aes(group = sentiment)) + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))


daily_sentiment_cases <- daily_sentiment %>% filter(sentiment == "negative") 

daily_sentiment_plot + geom_label_repel(aes(label = cases),data = daily_sentiment_cases, min.segment.length = 0.5, box.padding = 0.5) +
 ggsave("daily_sentiment_plot.png", dpi = 300)



#sentiment word rank(optional: filter date)
word_sentiment_rank <- data %>%
  unnest_tokens(output = "word", input = "segged",
                token = "regex", pattern = "\u3000")%>%
  #filter(date == "May 26")%>%
  group_by(word) %>%
  summarize(n = n()) %>%
  filter(n > 5) %>%
  arrange(desc(n))%>%
  inner_join(PN_adjusted)%>%
  distinct()%>%
  mutate(word = reorder(word, n))%>%
  top_n(30,wt = n) 

#plot for word_sentiment_rate
ggplot(word_sentiment_rank,aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  theme(text=element_text(size=12, family = "Heiti TC Light"))+
  coord_flip() + ggsave("word_sentiment_rate526.png", dpi = 300)
