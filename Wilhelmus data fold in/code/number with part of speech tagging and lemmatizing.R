#For section 6.1 of the first paper: this code is about average marginal frequencies for songs from 6 authors of Wilhelmus dataset


rm(list=ls())
library(Rcpp)
library(caret)
library(e1071)
library(stringr)
library(tm)
library("readxl")
library("superml")
library("udpipe")
library("data.table")
library("magrittr")
library("dplyr")
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(tidytext)
library(textstem)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
library("rJava")
library("qdap")

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}


# Define a function to read all files from a folder into a data frame
alldata_folder <- "C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/Wilhelmus data fold in/data/"
# alldata_folder <- "C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/add code/Wilhelmus dataset/"

# Use unnest() and map() to apply read_folder to each subfolder
alldata_raw_text <- tibble(folder = dir(alldata_folder, full.names = TRUE)) %>%
  mutate(folder_out = map(folder, read_folder)) %>%
  unnest(cols = c(folder_out)) %>%
  transmute(newsgroup = basename(folder), id, text)


alldata_usenet_words <- alldata_raw_text %>%
  unnest_tokens(word, text, token = stringr::str_split, pattern = " ")# %>%
   #filter(str_detect(word, "[a-z']$"),
         #!word %in% stop_words$word)

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

alldata_dtm <- as.matrix(alldata_dtm)
# alldata_word_newsgroups_id <- alldata_word_newsgroups %>%
#   filter(str_detect(newsgroup, "marnix")) %>%
#   count(word, top = 2)
dim(alldata_dtm)
sum(colSums(alldata_dtm) == 0)

category =c('datheen', 'marnix', 'heere', 'haecht', 'fruytiers', 'coornhert')

# Vocab size: number of terms in each author group

alldata_dtm_num <- as.data.frame(alldata_dtm)
alldata_dtm_num$id <- row.names(alldata_dtm_num)

for (i in 1:length(category)) {
  print(category[i])
  alldata_dtm_num_datheen <- alldata_dtm_num %>%
    filter(grepl(category[i], id))
  df = subset(alldata_dtm_num_datheen, select = -c(id))
  dfm <- as.matrix(df)
  dfmcol <- colSums(dfm)
  print(round(sum(dfmcol != 0),3))
}

# Avg. term corresponds with the average number of terms per song

alldata_dtm_num <- as.data.frame(rowSums(alldata_dtm != 0))
alldata_dtm_num$id <- row.names(alldata_dtm_num)
colnames(alldata_dtm_num) <- c("num", "id")
alldata_dtm_num[1,]

for (i in 1:length(category)) {
  print(category[i])
  alldata_dtm_num_datheen <- alldata_dtm_num %>%
    filter(grepl(category[i], id))
  print(round(mean(alldata_dtm_num_datheen$num),3))
  print(round(max(alldata_dtm_num_datheen$num)-mean(alldata_dtm_num_datheen$num),3))
  print(round(min(alldata_dtm_num_datheen$num)-mean(alldata_dtm_num_datheen$num),3))
}

print(round(mean(alldata_dtm_num$num),3))
print(round(max(alldata_dtm_num$num)-mean(alldata_dtm_num$num),3))
print(round(min(alldata_dtm_num$num)-mean(alldata_dtm_num$num),3))

# Avg. frequency indicates the average sum of term frequency per song: used for section 6.1 of the first paper
alldata_dtm_num <- as.data.frame(rowSums(alldata_dtm))
alldata_dtm_num$id <- row.names(alldata_dtm_num)
colnames(alldata_dtm_num) <- c("num", "id")
alldata_dtm_num[1,]


for (i in 1:length(category)) {
  print(category[i])
  alldata_dtm_num_datheen <- alldata_dtm_num %>%
    filter(grepl(category[i], id))
  print(round(mean(alldata_dtm_num_datheen$num),3))
  print(round(max(alldata_dtm_num_datheen$num)-mean(alldata_dtm_num_datheen$num),3))
  print(round(min(alldata_dtm_num_datheen$num)-mean(alldata_dtm_num_datheen$num),3))
}

print(round(mean(alldata_dtm_num$num),3))
print(round(max(alldata_dtm_num$num)-mean(alldata_dtm_num$num),3))
print(round(min(alldata_dtm_num$num)-mean(alldata_dtm_num$num),3))


for (i in 1:length(category)) {
  print(category[i])
  alldata_dtm_num_datheen <- alldata_dtm_num %>%
    filter(grepl(category[i], id))
  print(round((alldata_dtm_num_datheen$num),3))
}


path.test <- paste0("C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/Wilhelmus data fold in/data/", "/")
#path.test <- paste0("C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/Archive/data/", "/")
files.test <- list.files(path=path.test, pattern="wilhelmus.txt")
temp.test <- read.delim(paste(path.test,files.test,sep=""), header = FALSE)
colnames(temp.test) <- files.test
x.test <- temp.test

y.test <- character()
temp_vector.test <- as.vector(unlist(x.test))
y.test <- append(y.test, paste(temp_vector.test[1:length(temp_vector.test)], sep = "", collapse=" "))

#freq_terms((strsplit(y.test, " ")))
length(row.names(table(strsplit(y.test, " "))))
#strsplit(y.test, " ")
length(strsplit(y.test, " "))
y.test

row.names(table(strsplit(y.test, " ")))
sum(table(strsplit(y.test, " ")))

