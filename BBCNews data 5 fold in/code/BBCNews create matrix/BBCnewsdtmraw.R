#For section 5.3 of the first paper: this code is for 2225 documents from five categories using five-fold cross validation. Note each time the documents from the training set form a raw document-term matrix and the documents of validation set are not included in the matrix.

rm(list=ls())
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(stringr)
library(tidytext)
library(tm)
library(textstem)
library(caret)

#mfi <- 2000;

minfre <- 10;


# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}


# Define a function to read all files from a folder into a data frame
alldata_folder <- "C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/BBCNews data 5 fold in/data/bbcnews/"

# Use unnest() and map() to apply read_folder to each subfolder
alldata_raw_text <- tibble(folder = dir(alldata_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)

alldata_usenet_words <- alldata_raw_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

alldata_usenet_words$word <- lemmatize_words(alldata_usenet_words$word)


# include only words that occur at least 50 times
alldata_word_newsgroups <- alldata_usenet_words %>%
  filter(str_detect(newsgroup, "")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()

# convert into a document-term matrix
# with document names such as sci.crypt_14147
alldata_dtm <- alldata_word_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

category <- gsub('[.txt]', '', rownames(as.matrix(alldata_dtm))) 
category <- gsub('[^a-z]', '', category)
# category <-  removeNumbers(category)

category <- gsub('[_]', '', category)

set.seed(123)

fold_number <- 5
fold <- createFolds(category, k=fold_number)

fold$Fold1

fold_range <- 1:fold_number

training_dtm <- list()
testing_dtm <- list()
training_index <- list()
testing_index <- list()

for(n in fold_range){
  
  training_dtm[[n]] <- as.matrix(alldata_dtm)[-unlist(fold[n]), ]
  class(training_dtm[[n]])
  testing_dtm[[n]] <- as.matrix(alldata_dtm)[unlist(fold[n]), ]
  class(testing_dtm[[n]])
  
  # order_index <- order(-apply(training_dtm[[n]], 2, sum))
  # training_dtm.terms <- rownames(as.matrix(apply(training_dtm[[n]], 2, sum)[order_index[1:mfi]]))
  # training_dtm[[n]] <- subset(training_dtm[[n]], select=c(training_dtm.terms))
  
  training_dtm.terms <- colnames(training_dtm[[n]])[which(apply(training_dtm[[n]], 2, sum)>=minfre)]
  training_dtm[[n]] <- subset(training_dtm[[n]], select=c(training_dtm.terms))
  dim(training_dtm)
  
  dim(training_dtm[[n]])
  
  testing_dtm.terms <- colnames(testing_dtm[[n]])
  testing_dtm_row_names <- row.names(testing_dtm[[n]])
  
  testing_new_matrix <- matrix(rep(NA,nrow(testing_dtm[[n]])*ncol(training_dtm[[n]])), nrow = nrow(testing_dtm[[n]]), ncol = ncol(training_dtm[[n]]))
  
  for (j in 1:ncol(training_dtm[[n]])) {
    if (training_dtm.terms[j] %in% testing_dtm.terms) {
      testing_new_matrix[,j] <-  testing_dtm[[n]][, which(training_dtm.terms[j] == testing_dtm.terms)]
    } else {
      testing_new_matrix[,j] <-  0
    }
  }
  testing_dtm[[n]] <- testing_new_matrix
  row.names(testing_dtm[[n]]) <- testing_dtm_row_names
  colnames(testing_dtm[[n]]) <- training_dtm.terms
  
  print(which(apply(training_dtm[[n]], 1, sum) == 0))
  print(which(apply(testing_dtm[[n]], 1, sum) == 0))
  
  
  
  
  
  include_authors=c('business', 'entertainment', 'politics', 'sport', 'tech')
  
  # training_dtm[[n]] <- training_dtm[[n]][-which(apply(training_dtm[[n]], 1, sum) == 0), ]
  # testing_dtm[[n]] <- testing_dtm[[n]][-which(apply(testing_dtm[[n]], 1, sum) == 0), ]
  # dim(training_dtm[[n]])
  # dim(testing_dtm[[n]])
  
  training_index[[n]] <- row.names(training_dtm[[n]])
  
  for (i in 1:length(training_index[[n]])){
    if (grepl('business', training_index[[n]][i])) {
      training_index[[n]][i] <- 1
    } else if (grepl('entertainment', training_index[[n]][i])) {
      training_index[[n]][i] <- 2
    } else if (grepl('politics', training_index[[n]][i])) {
      training_index[[n]][i] <- 3
    } else if (grepl('sport', training_index[[n]][i])) {
      training_index[[n]][i] <- 4
    } else if (grepl('tech', training_index[[n]][i])) {
      training_index[[n]][i] <- 5
    }
  }
  
  testing_index[[n]] <- row.names(testing_dtm[[n]])
  for (i in 1:length(testing_index[[n]])){
    if (grepl('business', testing_index[[n]][i])) {
      testing_index[[n]][i] <- 1
    } else if (grepl('entertainment', testing_index[[n]][i])) {
      testing_index[[n]][i] <- 2
    } else if (grepl('politics', testing_index[[n]][i])) {
      testing_index[[n]][i] <- 3
    } else if (grepl('sport', testing_index[[n]][i])) {
      testing_index[[n]][i] <- 4
    } else if (grepl('tech', testing_index[[n]][i])) {
      testing_index[[n]][i] <- 5
    }
  }
  print(dim(training_dtm[[n]]))
  print(dim(testing_dtm[[n]]))
  
  
  # sum(training_index[[n]]==1)
  # sum(training_index[[n]]==2)
  # sum(training_index[[n]]==3)
  # sum(training_index[[n]]==4)
  # sum(training_index[[n]]==5)
  # 
  # sum(testing_index[[n]]==1)
  # sum(testing_index[[n]]==2)
  # sum(testing_index[[n]]==3)
  # sum(testing_index[[n]]==4)
  # sum(testing_index[[n]]==5)
  # 
  
}

save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\created matrix\\rawdtmBBCnews.Rdata")




