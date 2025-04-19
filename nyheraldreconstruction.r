library(tidyverse)
library(tidytext)
library(textdata)
library(readtext)
library(readr)
library(stringr)
library(textclean)
library(qdap)
library(hunspell)
library(tm)
library(tidytext)
library(quanteda)
library(topicmodels)
library(ldatuning)
library(text2vec)
library(textstem)
library(readtext)
library(widyr)
library(SnowballC)
library(purrr)
library(text2vec)
library(lsa)
library(dplyr)
library(textTinyR)
library(ggplot2)
library(quanteda.textstats)
#Not all of these libraries are reflected in the code because I had failed experiments. I attempted to clean the ocr, topic model, and various other methods.

df<-flattened_df#just while im working after pulling the json i use this
df <- read_csv("C:/Users/anton/Desktop/heraldtext/1870-1875.csv")
df<-df[,c("sequence", "date", "ocr_eng")]

clean_tokens %>%
  count(word, sort = TRUE)
  #total count shows most used words


word_tokens <- df %>%
  unnest_tokens(word, ocr_eng)
#after creating word_tokens the whole workflow operates without df
clean_tokens<- word_tokens %>%
  anti_join(stop_words)

clean_tokens<-clean_tokens%>%
  mutate(word_stem = wordStem(word))
  #stemming hasn't really been used in this coding

  #the code below is fantastic for checking the different spellings of words in the tokens
clean_tokens %>%
  filter(str_detect(word, "democrat")) %>% 
  group_by(word) %>%
   filter(word %in% c("democrat", "democrats")) %>%
  summarize(count = n())
  #alternative way to count words?



total.count.yearly<-read_csv("C:/Users/anton/Desktop/visualizations/total.count.yearly.csv")
total.count.monthly<- word_tokens %>%
mutate(month = substr(date, 1, 6)) %>%
#i did this by year as well
#to do by year change 6 to 4
group_by(month) %>%
summarize(total.words = n())


words.per.month<-word_tokens %>%
#i realized that if im filtering for specific words I don't need to use tokens without stop words
  mutate(month = substr(date, 1, 6)) %>%
  #this creates a column with only the year , allowing me to calculate number of words per year
  filter(word == "radical") %>%
#"democrat", "republican","reconstruction", "money", "labor", "economy", "freedmen","amnesty", "negro"
#"civil rights", "hard money", "negro suffrage", "soft money", "southern question", "southern states", "freedmens bureau"
     group_by(month) %>% 
     count()

word.freq<- words.per.month %>%
  left_join(total.count.monthly, by = "month") %>%
  mutate(wpy = n/ total.words)
  #creating a frequency column

word.freq <- word.freq%>%
  mutate(wpy_scaled = wpy * 100000)
  View(word.freq)


wpy_wide<-words.per.year %>%
pivot_wider(names_from = word , values_from = n)
#useful to have a wide data frame

x11()
ggplot(word.freq, aes(x = month , y = wpy_scaled))+geom_line(group=1)+
labs(title= "Radical Count Per 100,000 in NY Herald, 1870-1875 by Month", x = "Year", y = "Word Count Per 100,000")+scale_x_discrete(breaks=c(187001,187101,187201,187301,187401,187501))



total.count.monthly<- word_tokens %>%
mutate(month = substr(date, 1, 6)) %>%
group_by(month) %>%
summarize(total.words = n())
#total count of words by month to calculate frequencies per month

words.per.month<- clean_tokens %>%
  mutate(month = substr(date, 1, 6))%>%
  #word = gsub("s$", "", word)) %>%
  #this does month
  group_by(month, word) %>%
   filter(word %in% c("democrat", "republican","reconstruction", "money", "labor", "economy", "freedmen","amnesty", "negro")) %>%
    count()

View(head(word.freq))
word.freq<- words.per.month %>%
  left_join(total.count.monthly, by = "month") %>%
  mutate(wpy = n/ total.words)
#"republican", "democrat", "reconstruction", "money", "labor", "economy", "freedmen"
word.freq <- word.freq %>%
    filter(word %in% c("democrat","reconstruction", "labor", "economy", "freedmen","amnesty", "negro"))
  mutate(wpy_scaled = wpy * 1e6)


second.half<-clean_tokens %>%
mutate(year = substr(date, 1, 4))%>%
  filter(year %in% c(1873,1874,1875))%>%
  filter(!word %in% my_stops)%>%
  count(word, sort = TRUE) %>%
  head(n = 100)
  #second half of world cloud

second.half<-second.half%>%
filter(!word %in% my_stops)
 
#quick word cloud
top_100<-clean_tokens %>%
  count(word, sort = TRUE) %>%  
  head(n = 100)
top_100 <- top_100 %>%
  filter(!word %in% my_stops)

wordcloud2(first.half, size = 0.5, color = "random-light", backgroundColor = "black")

tok.by.year<-clean_tokens %>%
  mutate(year = substr(date, 1, 4))
#objective here is to create a df with the tokens and a year column so I can then paste all tokens back together grouped by year
grouped.by.year<-tok.by.year %>%
  group_by(year) %>%
  summarise(text = paste(word, collapse = " "))
#this pastes the tokens all back together but now separated by year

clean_tokens<- clean_tokens %>%
filter(!word %in% my_stops)
my_stops<- c( "ami", "street", "tho", "ol", "1", "2", "3", "4", "5", "6", "7", "8", "9", "lor", "o'clock", "bo", "aud", "day", "10", "st", "west", "john", "oi", "de", "ac", "ho", "12","tne", "11", "20", "tue", "lie", "tlie", "lo", "iu", "dr", "con", "ou", "01", "irom", "ot", "0", "la", "tor", "100", "ii", "tbe", "william", "lu","111", "ft", "city", "york", "twenty", "li","ten","time","late","house", "sale", "herald", "address", "east", "morning","30", "200","13", "fourth", "si", "16", "called", "18", "thirty", "ing", "avenue", "evening", "ana", "held", "17", "15", "00", "1874", "00", "50", "14", "1873", "night", "of",",",".","and","in","a","?", "for","i", "com", "i")



#all of the following code is used to make kwic word frequency counts, did not end up using in paper
grouped.by.year <- df %>%
mutate(year = substr(date, 1, 4)) %>%
#have to make a year column
  group_by(year) %>%
  summarise(text = paste(ocr_eng, collapse = " ")) 
  #this groups all the text into years
#look into making these lines into a function
corp.1875<-corpus(grouped.by.year %>%
  filter(year == "1875") %>%
  pull(text))
  #this creates corpus by year 

toks.1875<-quanteda::tokens(corp.1875)
#there are multiple token functions so have to tell R to use quanteda tokens

kwic.1875<-kwic(toks.1875, pattern = "reconstruction", window = 5)

repub.1875 <- as.data.frame(kwic.1875)
#turning it into a df to do some frequency analysis

fulltxt.1875<-paste(repub.1875$pre, repub.1875$post)
#turning the words before and after into a fulltext in order to tokenize

repub.tokens.1875<-quanteda::tokens(fulltxt.1875)
#tokenizing

repub.freq.1875 <- dfm(repub.tokens.1875)
#making frequency matrix

top.1875<-topfeatures(repub.freq.1875, n = 100)
#getting top 100 words by frequency
top.1875<- data.frame(
  word = names(top.1875),
  frequency = as.vector(top.1875))
#turn into table
clean<- function(top_data) {
  top_data <- top_data[!top_data$word %in% quanteda::stopwords("en"), ]
  top_data$word <- gsub("[[:punct:]]", "", top_data$word)
  top_data <- top_data[top_data$word != "" & trimws(top_data$word) != "", ]
  return(top_data)}

#made function to get rid of ocr errors and stop words ,getting rid of the useless words, my_stops is customized for these results

mystops<-c("ol","j","oi","ot","1","aud","tho","lor","t")
top.1875<-top.1875 %>% filter(!word %in% mystops)
top.1875<-clean(top.1875)




output_dir<-"C:/Users/anton/Desktop/individualtxt"
row_to_text<- function(row, output_dir) {
  file_path<-file.path(output_dir, paste0(row["date"], row["sequence"], ".txt"))
  
  content<-paste(
    paste("page:", row["sequence"]),
    paste("text:", row["ocr_eng"]),
    sep = "\n")

    writeLines(content, file_path)
  
}
apply(df, 1, function(row) row_to_text(as.data.frame(t(row)), output_dir))
#this was a function to write individual rows of text into separate txt files, basically to get each page by day into individual text files





file_path<-"C:/Users/anton/Desktop/ocr_texts"
ocr_newspaper<-paste0(file_path, "/", "herald1870", ".txt")
file_conn <- file(ocr_newspaper, "a")
#"a" adds or appends and "w" overwrites
#it wasn't working unless I opened a file connection with file_conn idk why

for(i in 1:length(organized_json)){
    ocr_text<-organized_json[[i]]$ocr_eng
    writeLines(ocr_text, file_conn)
}
close(file_conn)








text<-readLines("C:/Users/anton/Desktop/ocr_texts/herald1875p3.txt")
text <- gsub("[^[:alnum:]\\s]", "", text)  # Remove non-alphanumeric characters
ltext<-tolower(text)

clean_republican<-c("republic?we", "republica", "republicaes", "republican", "republican'", 
"republican!", "republican*", "republican**", "republican*.", 
"republican*?", "republican,", "republican.", "republican.*", 
"republican.,", "republican;", "republican?", "republican?do", 
"republican?some", "republican?yet,", "republicana", "republicans", "republicans*", 
"republicans,", "republicans.", "republicans;", 
"republicau", "republicaus", "republics", "republics,", "republics.", 
"republics:?", "republicum", "republicuu" , "republii", 
"republlcau", "republlcaus", "republlcn")
clean_democrat<- c("democrals", "democrat#", "democrat*", "democrat*?the", "democrat.c","democrat;","democrat?a","democrat?filth", "democrata", "democrate", "democrats)", 
"democrats;", "democrats?", "democrats?they", "democrut", "democrau", "democrui") 
#trying to clean up mentions of these words
 


#FOUND A WAY TO REPLCE MISSPELLINGS IN TOKEN LIST
correct<-"republican"
word_tokens <- word_tokens %>%
  mutate(word = if_else(word %in% clean_republican, correct, word))






# Tokenize into words
word_tokens <- text_df %>%
  unnest_tokens(word, text)
#this creates a dataframe of words, in line by line format

#powerful way to include potential misspellings or just different spellings
word_tokens %>%
  filter(str_detect(word, "^freedmen(s)?$")) %>%  # Regex to match words with s and no s
  count(word)
 
word_counts %>% 
filter(word== "republican") %>%
count(word)
#count words

View(word_tokens)


  word_tokens %>%
  filter(str_detect(word, "republican")) %>%
  count(word, sort = TRUE)
#checking different spellings of republican

word_tokens %>%
  filter(word == "republican") %>%
  distinct()
#checks for duplicates



#quick way to get counts
phrase <-"credit mobilier"

# Find all matches
matches <- grep(phrase, ltext)

# Count the number of matches
count <- length(matches)


print(count)


#topic modeling?
lda<- LDA(dtm, k = 25, control = list(seed = 12345), method = "Gibbs", alpha = .05)




txt<- readtext("C:/Users/anton/Desktop/herald1874.txt")

herald.tokens<- txt %>%
  unnest_tokens(sentence, text, token = "sentences") 
 #doesn't make sense to do stop words when im doing sentences


 herald.tokens<- herald.tokens %>%
  mutate(group = (row_number() - 1) %/% 3) %>%
  #only way I could figure to do this, this creates groups by every 10 rows
  group_by(group) %>%
  summarise(text = paste(sentence, collapse = " ")) 
  #paste the 10 sentence rows into one


tokens<- herald.tokens %>%
  unnest_tokens(word,text) %>%
    anti_join(stop_words)
herald.dtm <- tokens %>% 
  count(group, word) %>% 
  cast_dtm(group, word, n)
herald.lda<- LDA(herald.dtm, k = 5, control = list(seed = 12345), method = "Gibbs", alpha = .05)
herald.topics <- tidy(herald.lda, matrix = "beta")
herald.top.terms <- herald.topics %>%
  arrange(desc(beta)) %>% 
  group_by(topic) %>% slice(1:5)
  View(herald.top.terms)
 
ggplot(data=herald.top.terms, aes(x=term, y=beta)) + geom_bar(stat="identity") + facet_wrap(~ topic, scales = "free") + 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

words <- unlist(strsplit(ltext, "\\s+"))
#the line above and below can be used in combination to generate an ocr dictionary by looking at mispellings
word_freq <- table(words)
View(word_freq)
word_to_count<-"republican"
#simple way to do a word count, do a word frequency table then assign a word or words to variable, then subset the word to count in word frequency and the number will be the overlap
word_freq[word_to_count]









