library(ggplot2)
library(tm)
library(tidytext)
library(dplyr)
library(RWeka)
library(tidyr)
set.seed(233)

#seet the working directory
setwd("C:/Users/Siddharth/Documents/Careers/Siddharth/DataScienceSpecialization/Data Science Capstone/Final Project/final/en_US")

twitter.file <- file("en_US.twitter.txt")
twitter <- readLines(twitter.file)

blogs.file <- file("en_US.blogs.txt")
blogs <- readLines(blogs.file)


news.file <- file("en_US.news.txt")
news <- readLines(news.file)

twitter_df <- tibble(line = 1:2360148, text = twitter)
blogs_df <- tibble(line = 1:899288, text = blogs)
news_df <- tibble(line = 1:77259, text = news)

#split the data into a training and a test set
train_news <- news_df %>% sample_frac(0.75)
train_blogs <- blogs_df %>% sample_frac(0.75)
train_twitter <- twitter_df %>% sample_frac(0.75)

test_news <- anti_join(news_df, train_news, by = "line")
test_blogs <- anti_join(blogs_df, train_blogs, by = "line")
test_twitter <- anti_join(twitter_df, train_twitter, by = "line")

#break the training data into trigrams for each data frame
train_twitter_trigrams <- train_twitter %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
train_blogs_trigrams <- train_blogs %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
train_news_trigrams <- train_news %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)

#break the training data into bigrams for each data frame
train_twitter_bigrams <- train_twitter %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
train_blogs_bigrams <- train_blogs %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
train_news_bigrams <- train_news %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

#trigrams words separate
train_twitter_trigrams <- train_twitter_trigrams %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
train_blogs_trigrams <- train_blogs_trigrams %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")
train_news_trigrams <- train_news_trigrams %>% separate(trigram, c("word1", "word2", "word3"), sep = " ")

#bigrams words separate
train_twitter_bigrams <- train_twitter_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
train_blogs_bigrams <- train_blogs_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
train_news_bigrams <- train_news_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")

#Remove stop words from each of the three files for trigrams
train_twitter_trigrams <- train_twitter_trigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% 
        filter(!word3 %in% stop_words$word)
train_news_trigrams <- train_news_trigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% 
        filter(!word3 %in% stop_words$word)
train_blogs_trigrams <- train_blogs_trigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word) %>% 
        filter(!word3 %in% stop_words$word)

#Remove stop words from each of the three files for bigrams
train_twitter_bigrams <- train_twitter_bigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
train_news_bigrams <- train_news_bigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
train_blogs_bigrams <- train_blogs_bigrams %>% filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)

#sort the filtered trigrams in the decreasing order of frequency
train_blogs_trigrams <- train_blogs_trigrams %>% count(word1, word2, word3, sort = TRUE)
train_news_trigrams <- train_news_trigrams %>% count(word1, word2, word3, sort = TRUE)
train_twitter_trigrams <- train_twitter_trigrams %>% count(word1, word2, word3, sort = TRUE)

#sort the filtered bigrams in the decreasing order of frequency
train_blogs_bigrams <- train_blogs_bigrams %>% count(word1, word2, sort = TRUE)
train_news_bigrams <- train_news_bigrams %>% count(word1, word2, sort = TRUE)
train_twitter_bigrams <- train_twitter_bigrams %>% count(word1, word2, sort = TRUE)

#remove words that are not useful from the bigrams models
garbage_blogs <- grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|NA|ã|â|à|œ|ká|î|ï", train_blogs_bigrams$word1) |
        grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|NA|ã|â|à|œ|ká|î|ï", train_blogs_bigrams$word2)
        
train_blogs_bigrams<- train_blogs_bigrams[garbage_blogs == FALSE,]

garbage_news <- grepl("â|iâ|itâ|1|2|3|4|5|6|7|8|9|NA|ã|â", train_news_bigrams$word1) | 
        grepl("â|iâ|itâ|1|2|3|4|NA|ã|â", train_news_bigrams$word2)

train_news_bigrams <- train_news_bigrams[garbage_news == FALSE,]

garbage_twitter <- grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|_|NA|ã|â|ðÿ|rt", train_twitter_bigrams$word1) | 
        grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|_|NA|ã|â", train_twitter_bigrams$word2)

train_twitter_bigrams <- train_twitter_bigrams[garbage_twitter == FALSE,]

#remove words that are not useful from the trigrams models
garbage_blogs <- grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|NA|ã|â|à|œ|ká|î|ï|æ|é|á|ˆ", train_blogs_trigrams$word1) |
        grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|NA|ã|â|à|œ|ká|î|ï|æ|é|á", train_blogs_trigrams$word2) | 
        grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|NA|ã|â|à|œ|ká|î|ï|æ|é|á", train_blogs_trigrams$word3)

train_blogs_trigrams <- train_blogs_trigrams[garbage_blogs == FALSE,]

garbage_news <- grepl("â|iâ|itâ|1|2|3|4|5|6|7|8|9|NA|ã|â", train_news_trigrams$word1) | 
        grepl("â|iâ|itâ|1|2|3|4|NA|ã|â", train_news_trigrams$word2) | 
        grepl("â|iâ|itâ|1|2|3|4|NA|ã|â", train_news_trigrams$word3)

train_news_trigrams<- train_news_trigrams[garbage_news == FALSE,]

garbage_twitter <- grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|_|NA|ã|â|ðÿ|rt", train_twitter_trigrams$word1) | 
        grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|_|NA|ã|â", train_twitter_trigrams$word2) | 
        grepl("â|iâ|itâ|0|1|2|3|4|5|6|7|8|9|_|NA|ã|â", train_twitter_trigrams$word3)

train_twitter_trigrams <- train_twitter_trigrams[garbage_twitter == FALSE,]

#keeping only bigrams from blogs that occur more than 7 times. this is because a lot of the other observations include numbers or
#are meaningless
train_blogs_bigrams <- train_blogs_bigrams %>% filter(n > 7)
train_twitter_bigrams <- train_twitter_bigrams %>% filter(n > 4)
train_news_bigrams <- train_news_bigrams %>% filter(n > 5)

#train_word1_blogs <- table(unlist(train_blogs_bigrams_filtered$word1))
#word1_df <- as.data.frame(train_word1_blogs)

#sort the 3 training dataframes based on the first word in the bigrams
train_blogs_bigrams <- train_blogs_bigrams[order(train_blogs_bigrams$word1),]
train_news_bigrams <- train_news_bigrams[order(train_news_bigrams$word1),]
train_twitter_bigrams <- train_twitter_bigrams[order(train_twitter_bigrams$word1),]

#the for loop will loop through each row, finding all the rows where the  first word is present. It will then divide the number of
#instances where the second word occurs for the given first word, by the total number of instances where the first word occurs. This
#will give the weighted probability for the second word occuring, given the first word

for (i in 1:nrow(train_blogs_bigrams))
{
        abs <- train_blogs_bigrams[grep(paste0('^',train_blogs_bigrams$word1[i],'$'),train_blogs_bigrams$word1),]
        
        train_blogs_bigrams$pword2[i] <- train_blogs_bigrams$n[i] / sum(abs$n)
}

for (i in 1:nrow(train_news_bigrams))
{
        abs <- train_news_bigrams[grep(paste0('^',train_news_bigrams$word1[i],'$'),train_news_bigrams$word1),]
        
        train_news_bigrams$pword2[i] <- train_news_bigrams$n[i] / sum(abs$n)
}

for (i in 1:nrow(train_twitter_bigrams))
{
        abs <- train_twitter_bigrams[grep(paste0('^',train_twitter_bigrams$word1[i],'$'),train_twitter_bigrams$word1),]
        
        train_twitter_bigrams$pword2[i] <- train_twitter_bigrams$n[i] / sum(abs$n)
}

combined_bigrams <- rbind(train_blogs_bigrams, train_news_bigrams, train_twitter_bigrams)

write.csv(combined_bigrams,file = "Word Prediction.csv")

#remove the first row from the blogs trigrams since they are all NAs

train_blogs_trigrams <- train_blogs_trigrams[-1,]
train_twitter_trigrams <- train_twitter_trigrams[-1,]

#keeping only trigrams from blogs that occur more than 2 times. this is because a lot of the other observations include numbers or
#are meaningless
train_blogs_trigrams <- train_blogs_trigrams %>% filter(n > 2)
train_twitter_trigrams <- train_twitter_trigrams %>% filter(n > 2)
train_news_trigrams <- train_news_trigrams %>% filter(n > 3)

#sort the 3 training dataframes based on the first word in the bigrams
train_blogs_trigrams <- train_blogs_trigrams[order(train_blogs_trigrams$word1, train_blogs_trigrams$word2),]
train_news_trigrams <- train_news_trigrams[order(train_news_trigrams$word1, train_news_trigrams$word2),]
train_twitter_trigrams <- train_twitter_trigrams[order(train_twitter_trigrams$word1, train_twitter_trigrams$word2),]

#the for loop will loop through each row, finding all the rows where the  first word is present. It will then divide the number of
#instances where the second word occurs for the given first word, by the total number of instances where the first word occurs. This
#will give the weighted probability for the second word occuring, given the first word

for (i in 1:nrow(train_blogs_trigrams))
{
        abs <- train_blogs_trigrams[grep(paste0('^',train_blogs_trigrams$word1[i],'$'),train_blogs_trigrams$word1) & 
                                            grep(paste0('^',train_blogs_trigrams$word2[i],'$'),train_blogs_trigrams$word2),] 
        
        train_blogs_trigrams$pword3[i] <- train_blogs_trigrams$n[i] / sum(abs$n)
}

for (i in 1:nrow(train_twitter_trigrams))
{
        abs <- train_twitter_trigrams[grep(paste0('^',train_twitter_trigrams$word1[i],'$'),train_twitter_trigrams$word1) & 
                                            grep(paste0('^',train_twitter_trigrams$word2[i],'$'),train_twitter_trigrams$word2),] 
        
        train_twitter_trigrams$pword3[i] <- train_twitter_trigrams$n[i] / sum(abs$n)
}


for (i in 1:nrow(train_news_trigrams))
{
        abs <- train_news_trigrams[grep(paste0('^',train_news_trigrams$word1[i],'$'),train_news_trigrams$word1) & 
                                              grep(paste0('^',train_news_trigrams$word2[i],'$'),train_news_trigrams$word2),] 
        
        train_news_trigrams$pword3[i] <- train_news_trigrams$n[i] / sum(abs$n)
}

combined_trigrams <- rbind(train_blogs_trigrams, train_news_trigrams, train_twitter_trigrams)

write.csv(combined_trigrams, file = "Word Prediction Trigrams.csv")



