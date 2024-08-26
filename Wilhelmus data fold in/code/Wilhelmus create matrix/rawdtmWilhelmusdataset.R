#For section 6.3 of the first paper: this code is for songs from six authors using LOOCV. Note each time 185 documents from training set form a raw document-term matrix of size 185*300 and the single document of validation set is not included in the matrix.


rm(list=ls())
library(caret)
library(e1071)
library(stringr)
library(tm)
library("readxl")
library("superml")
library("udpipe")
library("data.table")

mfi <- 300;
include_authors=c('datheen', 'marnix', 'heere', 'haecht', 'fruytiers', 'coornhert')

x <- {}
author_len <- {}


for (i in 1:length(include_authors)) {
  path <- paste0("C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/Wilhelmus data fold in/data", "/", include_authors[i], "/")
  files <- list.files(path=path, pattern="*.txt")
  author_len[i] <- length(files)
  for (j in 1:length(files)) {
    temp <- read.delim(paste(path,files[j],sep=""), header = FALSE)
    colnames(temp) <- files[j]
    x <- append(x,temp)
  }
}

y <- character()
for (i in 1:length(x)) {
  temp_vector <- as.vector(unlist(x[[i]]))
  y <- append(y, paste(temp_vector[1:length(temp_vector)], sep = "", collapse=" "))
} 

author_datheen <- rep(c("datheen"), each = author_len[1])
author_marnix <- rep(c("marnix"), each = author_len[2])
author_heere <- rep(c("heere"), each = author_len[3])
author_haecht <- rep(c("haecht"), each = author_len[4])
author_fruytiers <- rep(c("fruytiers"), each = author_len[5])
author_coornhert <- rep(c("coornhert"), each = author_len[6])

view <- factor(c(author_datheen, author_marnix, author_heere, author_haecht, author_fruytiers, author_coornhert))
row.name.cm <- view
row.name.cm.num <- as.character(row.name.cm)
row.name.cm.num[which(row.name.cm ==include_authors[1])] <- 1
row.name.cm.num[which(row.name.cm ==include_authors[2])] <- 2
row.name.cm.num[which(row.name.cm ==include_authors[3])] <- 3
row.name.cm.num[which(row.name.cm ==include_authors[4])] <- 4
row.name.cm.num[which(row.name.cm ==include_authors[5])] <- 5
row.name.cm.num[which(row.name.cm ==include_authors[6])] <- 6


loo.len  = 1:length(y)
training_dtm <- list()
testing_dtm <- list()
training_index <- list()
testing_index <- list()

for(i in loo.len){
  df.train <- data.frame(y[-i], view[-i], stringsAsFactors = FALSE)
  corpus.train <- Corpus(VectorSource(df.train$y))
  td.mat.train <- DocumentTermMatrix(corpus.train)
  td.mat.train <- as.matrix(td.mat.train)
  order_index <- order(-apply(td.mat.train, 2, sum))
  training_dtm.terms <- rownames(as.matrix(apply(td.mat.train, 2, sum)[order_index[1:mfi]]))
  training_dtm[[i]] <- subset(td.mat.train, select=c(training_dtm.terms))
  dim(training_dtm[[i]])
  class(training_dtm[[i]])
  row.names(training_dtm[[i]]) <- view[-i]

  training_index[[i]] <- row.name.cm.num[-i]
  testing_index[[i]] <- row.name.cm.num[i]
  
  df.test <- data.frame(y[i], view[i], stringsAsFactors = FALSE)
  test_corpus <- Corpus(VectorSource(df.test[[1]]))
  test_td.mat <- DocumentTermMatrix(test_corpus)
  test_td.mat_terms <- as.matrix(findFreqTerms(test_td.mat, 0))
  test_td.mat <- as.matrix(test_td.mat)

  test_new_matrix <- matrix(NA,1,mfi)
  for (j in 1:mfi) {
    if (training_dtm.terms[j] %in% test_td.mat_terms) {
      test_new_matrix[1,j] <-  subset(test_td.mat, select=c(training_dtm.terms[j]))
    } else {
      test_new_matrix[1,j] <-  0
    }
  }
  testing_dtm[[i]] <- test_new_matrix
  row.names(testing_dtm[[i]]) <- view[i]
  colnames(testing_dtm[[i]]) <- training_dtm.terms
  dim(testing_dtm[[i]])
}


 
save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\created matrix\\rawdtmWilhelmusdataset.Rdata")
