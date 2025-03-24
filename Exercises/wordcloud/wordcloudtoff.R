
##################################

rm(list=ls())
df = read.csv("scopustoff.csv")
library(wordcloud)

##################################

text = paste(c(df$Title,df$Abstract,df$Author.Keywords),collapse=" ")
text = tolower(text)

text = gsub("[[:punct:]]","",text)
text = gsub("[[:digit:]]", "", text)
stopwords = c("the", "and", "to", "of", "a", "in", "that", "is", 
              "on", "for", "with", "as", "it", "was", "at", "by", 
              "an", "are", "be", "this", "or", "from", "which", 
              "not", "but", "also", "has", "have", "had", "were", 
              "their", "will", "can", "if", "would")

words = unlist(strsplit(text, " "))
words = words[!words %in% stopwords] 
words = words[nchar(words) > 2]

word_freq = data.frame(table(words))

wordcloud(words = word_freq$words, 
          freq = word_freq$Freq, 
          min.freq = 3,        
          max.words = 200,      
          random.order = FALSE,  
          colors = brewer.pal(8, "Dark2"))  


##################################

