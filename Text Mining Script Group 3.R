# Group 3
# Data 630
# Dr. G
# UMGC Final Project

#------------------------------------------------------------------------------------------

# Section 1 - Load and view data

# Set working directory
setwd("~/Desktop/Data 630/csv files")

# Import dataset "HR.csv"
hr = read.csv("HR.csv", 
              header = T,
              stringsAsFactors = F)

# Examine dataset
summary(hr)

# Load libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("ggplot2")
library("RColorBrewer")

#------------------------------------------------------------------------------------------

# Section 2 - Build corpus

# Assign data to Corpus
review_corpus = Corpus(VectorSource(hr$reviews.text))

# Remove numbers and punctuation
review_corpus = tm_map(review_corpus, 
                       removeNumbers)
review_corpus = tm_map(review_corpus, 
                       removePunctuation)

# Change all words to lowercase
review_corpus = tm_map(review_corpus, 
                       tolower)

# Remove English stop words
review_corpus = tm_map(review_corpus, removeWords, c("hotel", 
                                                     "room", 
                                                     "rooms", 
                                                     "stay", 
                                                     "stayed", 
                                                     "just", 
                                                     stopwords("english")))

# Strip white space
review_corpus = tm_map(review_corpus, 
                       stripWhitespace)

# View review 2 to ensure all reductions have been done correctly
inspect(review_corpus[2])

#------------------------------------------------------------------------------------------

# Section 3 - Build DTM

# Convert "review_corpus" to DTM
review_dtm = DocumentTermMatrix(review_corpus)

# Remove sparse terms
review_dtm = removeSparseTerms(review_dtm, 
                               0.98)

# Convert to matrix and ensure it has been converted
freq = colSums(as.matrix(review_dtm))
freq

# Sort matrix decreasing and see top 20 most frequent words
freq_sorted = sort(colSums(as.matrix(review_dtm)), decreasing = T)
head(freq_sorted, 20)

#------------------------------------------------------------------------------------------

# Section 4 - Plot and build word cloud

# Change to data frame for ggplot2
df_words = data.frame(word = names(freq_sorted), 
                      freq = freq_sorted)

# Assign ggplot to p
p = ggplot(subset(df_words[1:20,], 
                  freq>50), 
             aes(x = reorder(word, -freq), y = freq)) +
             geom_bar(stat = "identity") + 
             theme(axis.text.x = element_text(angle=45, 
                                              hjust=1))

# Plot the graph
p

# Build df for word cloud and plot top 20 words
freq_wc = data.frame(sort(colSums(as.matrix(review_dtm)), 
                          decreasing = TRUE))
wordcloud(rownames(freq_wc), 
          freq_sorted, 
          min.freq = 3284, 
          max.words = 50, 
          colors = c("pink", "green", "blue"))

# End of Script