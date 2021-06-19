library(quanteda)

data_corpus <- read_csv("./PTT疫苗1.csv")

new_word_list <- scan("./dic.txt" ,character(), quote = "")
writeLines(new_word_list, "./dict2.txt")  
stop_word_list <- scan("./stop.txt" ,character(), quote = "")
writeLines(stop_word_list, "./stop2.txt")  

seg <- worker(user = "./dict2.txt",stop_word = "./stop2.txt")


segment(data_corpus$content, seg)

#create a new vector and segment the content
data_segged <- vector("character", length = length(data_corpus$content))
for (i in seq_along(data_corpus$content)){
  segged <- segment(data_corpus$content[i], seg)
  
  # Collapse the character vector into a string, separated by space
  data_segged[i] <- paste0(segged, collapse = "\u3000")
}

#add the segged vector as a new column in data
data_corpus$segged <- data_segged


quanteda_corpus <- corpus(data_corpus, 
                          docid_field = "PTT疫苗1", 
                          text_field = "segged")

corpus_tokens <- tokenizers::tokenize_regex(quanteda_corpus, "\u3000") %>%
  tokens()

a <- kwic(corpus_tokens, "取得", window = 5, valuetype = "regex")

