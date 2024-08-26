#For section 5.3 of the first paper: this code is for 2963 documents from three categories where 1779 documents are from training set and 1184 documents are from validation set. Note the documents from the training set form a raw document-term matrix and the documents of validation set are not included in the matrix.


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
training_dtm <- training_word_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

training_dtm.terms <- findFreqTerms(training_dtm, minfre)
training_dtm <- as.matrix(training_dtm)
training_dtm <- subset(training_dtm, select=c(training_dtm.terms))
dim(training_dtm)


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

testing_dtm <- testing_word_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

testing_dtm.terms <- as.matrix(findFreqTerms(testing_dtm, 0))
testing_dtm <- as.matrix(testing_dtm)
testing_dtm_row_names <- row.names(testing_dtm)

testing_new_matrix <- matrix(rep(NA,nrow(testing_dtm)*ncol(training_dtm)), nrow = nrow(testing_dtm), ncol = ncol(training_dtm))

for (j in 1:ncol(training_dtm)) {
  if (training_dtm.terms[j] %in% testing_dtm.terms) {
    testing_new_matrix[,j] <-  testing_dtm[, which(training_dtm.terms[j] == testing_dtm.terms)]
  } else {
    testing_new_matrix[,j] <-  0
  }
}
testing_dtm <- testing_new_matrix
row.names(testing_dtm) <- testing_dtm_row_names
colnames(testing_dtm) <- training_dtm.terms

dim(training_dtm)
dim(testing_dtm)

training_dtm <- training_dtm[-which(apply(training_dtm, 1, sum) == 0), ]
testing_dtm <- testing_dtm[-which(apply(testing_dtm, 1, sum) == 0), ]

dim(training_dtm)
dim(testing_dtm)

training_index <- row.names(training_dtm)

for (i in 1:length(training_index)){
  if (grepl('comp.graphics', training_index[i])) {
    training_index[i] <- 1
  } else if (grepl('rec.sport.hockey', training_index[i])) {
    training_index[i] <- 2
  } else if (grepl('sci.crypt', training_index[i])) {
    training_index[i] <- 3
  }
}

testing_index <- row.names(testing_dtm)
for (i in 1:length(testing_index)){
  if (grepl('comp.graphics', testing_index[i])) {
    testing_index[i] <- 1
  } else if (grepl('rec.sport.hockey', testing_index[i])) {
    testing_index[i] <- 2
  } else if (grepl('sci.crypt', testing_index[i])) {
    testing_index[i] <- 3
  }
}
save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\created matrix\\rawdtm20newsgroups.Rdata")
