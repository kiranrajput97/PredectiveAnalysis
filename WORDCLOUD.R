
library("wordcloud")     # word-cloud generator 
library("SnowballC")    # for text stemming
library("tm")    # for text mining
library("RColorBrewer")
# Read the text file
text <- readLines(file.choose())
# Load the data as a corpus
dataset<- Corpus(VectorSource(text))
# Remove punctuations
dataset <- tm_map(dataset, removePunctuation)
# Remove english common stopwords
dataset <- tm_map(dataset, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
dataset <- tm_map(dataset, removeWords, c("blabla1", "blabla2")) 
dataset <- tm_map(dataset, stemDocument) 
#wordcloud(dataset,max.words = 100,random.order = FALSE)
wordcloud(dataset,max.words = 100,random.order = FALSE,colors=brewer.pal(8, "Dark2"))

