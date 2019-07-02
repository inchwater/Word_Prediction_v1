library(dplyr)
library(xlsx)
#setwd("C:/Users/Siddharth/Documents/Careers/Siddharth/DataScienceSpecialization/Data Science Capstone/Final Project/final/en_US/")

#load the previously pre-processed data. this data includes 2 files, one for a bigram model and the other for a trigram model
bigrams <- read.csv("./Word Prediction.csv", sep = ',', fileEncoding = 'latin1')
trigrams <- read.csv("./Word Prediction Trigrams.csv", sep = ',', fileEncoding = 'latin1')

bigrams <- as_tibble(bigrams)
trigrams <- as_tibble(trigrams)

server <- function(input, output){
        p <-  reactive({
                l <- sapply(strsplit(input$words, " "), length)
                allwords <- data.frame()
                allwords <- (scan(text = input$words, what = ""))
                #allwords <- allwords %>% rename(words = `scan(text = words, what = "")`)
                
                if (l == 1){
                        probs_bi <- bigrams[bigrams$word1 == paste0(input$words),]
                        rowmax <- which.max(probs_bi$pword2)
                        as.character(probs_bi[[paste0(rowmax),3]])
                }
                else if (l > 2){
                        word1 <- allwords[l-2]
                        word2 <- allwords[l-1]
                        probs_tri_word1 <- trigrams[trigrams$word1 == paste0(word1),]
                        probs_tri <- probs_tri_word1[probs_tri_word1$word2 == paste0(word2),]
                        if (is.na(probs_tri$word1) == TRUE){
                                print ("No suitable word prediction found")
                        }
                        else{
                                rowmax <- which.max(probs_tri$pword3)
                                as.character(probs_tri[[paste0(rowmax),4]])
                        }
                }
                
                else{
                        print("No suitable word found")
                }
                
                      
                        
                        })
        output$predicted <- renderText({ p() })
                        
}


        


        

