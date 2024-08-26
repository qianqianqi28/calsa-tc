#For Figure 5 of section 5.2 of the first paper: this code is about creating raw document-term matrix for 2963 documents from 3 categories.


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

minfre <- 10

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}


training_folder <- "C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/20Newsgroups fold in/data/20news-bydate-train/used for experiments/"

# Use unnest() and map() to apply read_folder to each subfolder
training_raw_text <- tibble(folder = dir(training_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)

# must occur after the first occurrence of an empty line,
# and before the first occurrence of a line starting with --
training_cleaned_text <- training_raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()

training_cleaned_text <- training_cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <")
  )

training_usenet_words <- training_cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

training_usenet_words$word <- lemmatize_words(training_usenet_words$word)


# include only words that occur at least 50 times
training_word_newsgroups <- training_usenet_words %>%
  filter(str_detect(newsgroup, "")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()

# convert into a document-term matrix
# with document names such as sci.crypt_14147
# training_dtm <- training_word_newsgroups %>%
#   unite(document, newsgroup, id) %>%
#   count(document, word) %>%
#   cast_dtm(document, word, n)
# 
# training_dtm.terms <- findFreqTerms(training_dtm, minfre)
# training_dtm <- as.matrix(training_dtm)
# training_dtm <- subset(training_dtm, select=c(training_dtm.terms))
# dim(training_dtm)


testing_folder <- "C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/20Newsgroups fold in/data/20news-bydate-test/used for experiments/"

# Use unnest() and map() to apply read_folder to each subfolder
testing_raw_text <- tibble(folder = dir(testing_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)


# must occur after the first occurrence of an empty line,
# and before the first occurrence of a line starting with --
testing_cleaned_text <- testing_raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()

testing_cleaned_text <- testing_cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <")
  )

testing_usenet_words <- testing_cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

testing_usenet_words$word <- lemmatize_words(testing_usenet_words$word)


# include only words that occur at least 50 times
testing_word_newsgroups <- testing_usenet_words %>%
  filter(str_detect(newsgroup, "")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup()

# convert into a document-term matrix
# with document names such as sci.crypt_14147

# testing_dtm <- testing_word_newsgroups %>%
#   unite(document, newsgroup, id) %>%
#   count(document, word) %>%
#   cast_dtm(document, word, n)


alldata_word_newsgroups <- rbind(training_word_newsgroups, testing_word_newsgroups)

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

include_authors=c('comp.graphics', 'rec.sport.hockey', 'sci.crypt')

# training_dtm <- training_dtm[-which(apply(training_dtm, 1, sum) == 0), ]
# testing_dtm <- testing_dtm[-which(apply(testing_dtm, 1, sum) == 0), ]
# dim(training_dtm)
# dim(testing_dtm)
dtm <- dtm[-which(apply(dtm, 1, sum) == 0), ]
index <- row.names(dtm)

for (i in 1:length(index)){
  if (grepl('comp.graphics', index[i])) {
    index[i] <- 1
  } else if (grepl('rec.sport.hockey', index[i])) {
    index[i] <- 2
  } else if (grepl('sci.crypt', index[i])) {
    index[i] <- 3
  }
}


dim(dtm)



sum(index==1)
sum(index==2)
sum(index==3)




save(dtm, index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\figure\\20Newsgroups visualization\\rawdtm20Newsgroupsvisu.Rdata")



