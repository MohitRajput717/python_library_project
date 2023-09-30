rm(list=setdiff(ls(), c("i","status","d.sendmail","filepaths","timings")))
setwd("C:\\Users\\Praveen Sharma\\Downloads\\archive\\")
library(RMySQL)
library(lubridate)
library(excel.link)
library(BI)
library(dplyr)
library(data.table)
library(tcltk)
library(data.table)
library(dplyr)
library(tm)
library(wordcloud)
library(SnowballC)


df <- read.csv('movies_tmdb_popular.csv')
na <-is.na(df)

sapply(df, class)
#remove null values
df1 <- na.omit(df)
tail(df)

summary(df1)

#check the dataframe top 10
head(df1,10)

#find out langaue type and count
as.data.frame(table(df1$original_lang))
#find unique values
unique(df1$original_lang)

# Assuming df1 is your data frame
max_values <- sapply(df1, max, na.rm = TRUE)
min_values <- sapply(df1, function(x) min(x, na.rm = TRUE))





Hindi_movies <- subset(df1, original_lang == 'hi')

top_hindi_movies <- Hindi_movies[order(-Hindi_movies$popularity), ]
top_10_hindi_movies <- head(top_hindi_movies, 10)
cat("Hindi Movies with highest popularity are:\n")
cat(top_10_hindi_movies$title, "\n")


top_hindi_movies_by_vote_count <- Hindi_movies[order(-Hindi_movies$vote_count), ]
top_10_hindi_movies_by_vote_count <- head(top_hindi_movies_by_vote_count, 10)
cat("Hindi Movies with highest vote count are:\n")
cat(top_10_hindi_movies_by_vote_count$title, "\n")

top_hindi_movies_by_vote_average <- Hindi_movies[order(-Hindi_movies$vote_average), ]
top_10_hindi_movies_by_vote_average <- head(top_hindi_movies_by_vote_average, 10)
cat("Hindi Movies with highest vote average are:\n")
cat(top_10_hindi_movies_by_vote_average$title, "\n")



# Assuming df5 is your data frame
text <- as.character(Hindi_movies$title)

# Create a Corpus
corpus <- Corpus(VectorSource(text))

# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Create a word cloud
wordcloud(words = dtm$dimnames$Terms, freq = colSums(as.matrix(dtm)), scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

# Set title and axis off
title("Movies Title")
axis("off")

colnames(Hindi_movies)
colnames(df1)

