#For Figure 8 of section 6.2 of the first paper: this code is about creating raw document-term matrix for songs from two authors Datheen and Coornhert and creating the vector about the Wilhelmus. Note the Wilhelmus is not included in the matrix.


rm(list=ls())
library(caret)
library(e1071)
library(stringr)
library(tm)
library("readxl")
library("superml")
library("udpipe")
library("data.table")
library(Matrix)



mfi <- 300;
include_authors=c('datheen', 'coornhert')
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


view <- factor(c(author_datheen, author_marnix))

author_datheen.vector <- rep(c("da"), each = author_len[1])
author_marnix.vector <- rep(c("ma"), each = author_len[2])

view.vector <- (c(author_datheen.vector, author_marnix.vector))

df.train <- data.frame(y, view, stringsAsFactors = FALSE)
corpus.train <- Corpus(VectorSource(df.train$y))
td.mat.train <- DocumentTermMatrix(corpus.train)
td.mat.train <- as.matrix(td.mat.train)
order_index <- order(-apply(td.mat.train, 2, sum))
training_dtm.terms <- rownames(as.matrix(apply(td.mat.train, 2, sum)[order_index[1:mfi]]))
training_dtm <- subset(td.mat.train, select=c(training_dtm.terms))
dim(training_dtm)
class(training_dtm)
row.names(training_dtm) <- view

path.test <- paste0("C:/Users/qi000005/OneDrive - Universiteit Utrecht/qi000005/paper 1/20220417 Review 2 archive/Wilhelmus data fold in/data", "/")
files.test <- list.files(path=path.test, pattern="wilhelmus.txt")
temp.test <- read.delim(paste(path.test,files.test,sep=""), header = FALSE)
colnames(temp.test) <- files.test
x.test <- temp.test

y.test <- character()
temp_vector.test <- as.vector(unlist(x.test))
y.test <- append(y.test, paste(temp_vector.test[1:length(temp_vector.test)], sep = "", collapse=" "))
view.test <- factor(c("wilhelmus"))

df.test <- data.frame(y.test, view.test, stringsAsFactors = FALSE)
test_corpus <- Corpus(VectorSource(df.test$y.test))
test_td.mat <- DocumentTermMatrix(test_corpus)
test_td.mat_terms <- as.matrix(findFreqTerms(test_td.mat, 0))
test_td.mat <- as.matrix(test_td.mat)
dim(test_td.mat)
test_new_matrix <- matrix(NA,1,mfi)
for (j in 1:mfi) {
  if (training_dtm.terms[j] %in% test_td.mat_terms) {
    test_new_matrix[1,j] <-  subset(test_td.mat, select=c(training_dtm.terms[j]))
  } else {
    test_new_matrix[1,j] <-  0
  }
}
testing_dtm <- test_new_matrix
row.names(testing_dtm) <- view[i]
colnames(testing_dtm) <- training_dtm.terms
dim(testing_dtm)


save(training_dtm, testing_dtm,author_datheen.vector,  author_marnix.vector,file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\figure\\Wilhelmus visualization\\DathHeeHaeFruCooCA\\rawmatrixdathcoor.Rdata")
