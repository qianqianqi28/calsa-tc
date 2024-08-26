#For Figure 4 of section 5.2 of the first paper: this code is about creating raw document-term matrix for 737 documents from 5 categories.

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

minfre <- 10;

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}


# Define a function to read all files from a folder into a data frame
alldata_folder <- "C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/BBCSport data 5 fold in/data/bbcsport/"

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
category <- gsub('[_]', '', category)


idx <- category

dtm <- as.matrix(alldata_dtm)

# order_index <- order(-apply(dtm, 2, sum))
# training_dtm.terms <- rownames(as.matrix(apply(dtm, 2, sum)[order_index[1:mfi]]))
# dtm <- subset(dtm, select=c(training_dtm.terms))
# dim(dtm)
dtm.terms <- colnames(dtm)[which(apply(dtm, 2, sum)>=minfre)]
dtm <- subset(dtm, select=c(dtm.terms))
dim(dtm)


which(apply(dtm, 1, sum) == 0)

include_authors=c('athletics', 'cricket', 'football', 'rugby', 'tennis')

# training_dtm <- training_dtm[-which(apply(training_dtm, 1, sum) == 0), ]
# testing_dtm <- testing_dtm[-which(apply(testing_dtm, 1, sum) == 0), ]
# dim(training_dtm)
# dim(testing_dtm)

index <- row.names(dtm)

for (i in 1:length(index)){
  if (grepl('athletics', index[i])) {
    index[i] <- 1
  } else if (grepl('cricket', index[i])) {
    index[i] <- 2
  } else if (grepl('football', index[i])) {
    index[i] <- 3
  } else if (grepl('rugby', index[i])) {
    index[i] <- 4
  } else if (grepl('tennis', index[i])) {
    index[i] <- 5
  }
}


dim(dtm)



sum(index==1)
sum(index==2)
sum(index==3)
sum(index==4)
sum(index==5)



save(dtm, index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\figure\\BBCSport visualization\\rawdtmBBCsportvisu.Rdata")



