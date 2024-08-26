#For section 6.4 of the first paper: this code is to predict the attribution of the Wilhelmus using CA and centroid method


rm(list=ls())
library(caret)
library(e1071)
library(stringr)
library(tm)
library("readxl")
library("superml")
library("udpipe")
library("data.table")

caprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\centroid linkage\\cacentroidlinkage.csv",as.is = TRUE)


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

loo.len  = 1:length(y)


df.total <- data.frame(y, view, stringsAsFactors = FALSE)
corpus.total <- Corpus(VectorSource(df.total$y))
td.mat.total <- DocumentTermMatrix(corpus.total)
td.mat_mfi.total <- as.matrix(td.mat.total)
order_index <- order(-apply(td.mat_mfi.total, 2, sum))
td.mat_mfi_terms.total <- rownames(as.matrix(apply(td.mat_mfi.total, 2, sum)[order_index[1:mfi]]))
td.mat_mfi_choose.total <- subset(td.mat_mfi.total, select=c(td.mat_mfi_terms.total))
dim(td.mat_mfi_choose.total)
row.names(td.mat_mfi_choose.total) <- view
td.mat_mfi_choose.total <- as.matrix(td.mat_mfi_choose.total)

###
td.mat_mfi_choose.total.P    <- td.mat_mfi_choose.total/sum(td.mat_mfi_choose.total)
### Row and column masses
td.mat_mfi_choose.total.r    <- apply(td.mat_mfi_choose.total.P, 1, sum)
td.mat_mfi_choose.total.c    <- apply(td.mat_mfi_choose.total.P, 2, sum)
### CA Step 1: the matrix S
td.mat_mfi_choose.total.Dr   <- diag(td.mat_mfi_choose.total.r)
td.mat_mfi_choose.total.Dc   <- diag(td.mat_mfi_choose.total.c)
td.mat_mfi_choose.total.Drmh <- diag(1/sqrt(td.mat_mfi_choose.total.r))
td.mat_mfi_choose.total.Dcmh <- diag(1/sqrt(td.mat_mfi_choose.total.c))
td.mat_mfi_choose.total.P   <- as.matrix(td.mat_mfi_choose.total.P)

total_td.mat_mfi_choose.SR   <- td.mat_mfi_choose.total.Drmh%*%(td.mat_mfi_choose.total.P-td.mat_mfi_choose.total.r%o%td.mat_mfi_choose.total.c)%*%td.mat_mfi_choose.total.Dcmh
td.mat_mfi_choose.total.svd <- svd(total_td.mat_mfi_choose.SR)
training_dtm.csc <- td.mat_mfi_choose.total.Dcmh%*%td.mat_mfi_choose.total.svd$v


td.mat_mfi_choose.train <- td.mat_mfi_choose.total


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
test_corpus <- Corpus(VectorSource(df.test[[1]]))
test_td.mat <- DocumentTermMatrix(test_corpus)
test_td.mat_terms <- as.matrix(findFreqTerms(test_td.mat, 0))
test_td.mat <- as.matrix(test_td.mat)

test_new_matrix <- matrix(NA,1,mfi)
for (j in 1:mfi) {
  if (td.mat_mfi_terms.total[j] %in% test_td.mat_terms) {
    test_new_matrix[1,j] <-  subset(test_td.mat, select=c(td.mat_mfi_terms.total[j]))
  } else {
    test_new_matrix[1,j] <-  0
  }
}






qqadddat <- 0
qqaddmar <- 0
qqaddhee <- 0
qqaddhae <- 0
qqaddfru <- 0
qqaddcoo <- 0

qqindex = which(caprecionrecall$x==caprecionrecall$x[which.max(caprecionrecall$x)],arr.ind=T)
qqindex
length(qqindex)
k = 151
for (k in qqindex){

td.mat_mfi_choose.total.svd.ud <- as.matrix((td.mat_mfi_choose.total.Drmh%*%td.mat_mfi_choose.total.svd$u%*%diag(td.mat_mfi_choose.total.svd$d))[,1:k])
td.mat_mfi_choose.svd.ud <- as.matrix(td.mat_mfi_choose.total.svd.ud)

temp <- test_new_matrix/sum(test_new_matrix)

test_new_matrix.svd.ud <- t(as.matrix((as.matrix(temp) %*%as.matrix(training_dtm.csc))[,1:k]))



td.mat_mfi_choose.train.datheen <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="datheen",])
td.mat_mfi_choose.train.marnix <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="marnix",])
td.mat_mfi_choose.train.heere <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="heere",])
td.mat_mfi_choose.train.haecht <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="haecht",])
td.mat_mfi_choose.train.fruytiers <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="fruytiers",])
td.mat_mfi_choose.train.coornhert <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="coornhert",])

centroidlinkage.td.mat_mfi_choose.train.datheen <- t(as.matrix(apply(td.mat_mfi_choose.train.datheen,2,mean)))
centroidlinkage.td.mat_mfi_choose.train.marnix <- t(as.matrix(apply(td.mat_mfi_choose.train.marnix,2,mean)))
centroidlinkage.td.mat_mfi_choose.train.heere <- t(as.matrix(apply(td.mat_mfi_choose.train.heere,2,mean)))
centroidlinkage.td.mat_mfi_choose.train.haecht <- t(as.matrix(apply(td.mat_mfi_choose.train.haecht,2,mean)))
centroidlinkage.td.mat_mfi_choose.train.fruytiers <- t(as.matrix(apply(td.mat_mfi_choose.train.fruytiers,2,mean)))
centroidlinkage.td.mat_mfi_choose.train.coornhert <- t(as.matrix(apply(td.mat_mfi_choose.train.coornhert,2,mean)))


centroidlinkage_Eu_query_train.datheen <- sqrt(sum((test_new_matrix.svd.ud-centroidlinkage.td.mat_mfi_choose.train.datheen)^2))
centroidlinkage_Eu_query_train.marnix <- sqrt(sum((test_new_matrix.svd.ud-centroidlinkage.td.mat_mfi_choose.train.marnix)^2))
centroidlinkage_Eu_query_train.heere <- sqrt(sum((test_new_matrix.svd.ud-centroidlinkage.td.mat_mfi_choose.train.heere)^2))
centroidlinkage_Eu_query_train.haecht <- sqrt(sum((test_new_matrix.svd.ud-centroidlinkage.td.mat_mfi_choose.train.haecht)^2))
centroidlinkage_Eu_query_train.fruytiers <- sqrt(sum((test_new_matrix.svd.ud-centroidlinkage.td.mat_mfi_choose.train.fruytiers)^2))
centroidlinkage_Eu_query_train.coornhert <- sqrt(sum((test_new_matrix.svd.ud-centroidlinkage.td.mat_mfi_choose.train.coornhert)^2))


centroidlinkage.Eu = c(0,0,0,0,0,0);

centroidlinkage.Eu[1] <- centroidlinkage_Eu_query_train.datheen
centroidlinkage.Eu[2] <- centroidlinkage_Eu_query_train.marnix
centroidlinkage.Eu[3] <- centroidlinkage_Eu_query_train.heere
centroidlinkage.Eu[4] <- centroidlinkage_Eu_query_train.haecht
centroidlinkage.Eu[5] <- centroidlinkage_Eu_query_train.fruytiers
centroidlinkage.Eu[6] <- centroidlinkage_Eu_query_train.coornhert

print(k)

centroidlinkage.min.index <- which.min(centroidlinkage.Eu)
print(centroidlinkage.min.index)
centroidlinkage.size.min <- length(which(centroidlinkage.Eu==centroidlinkage.Eu[which.min(centroidlinkage.Eu)],arr.ind=T))
print(centroidlinkage.size.min)

centroidlinkage.chechhaecht.Eu <- centroidlinkage.Eu[2:6]

centroidlinkage.chechhaecht.min.index <- which.min(centroidlinkage.chechhaecht.Eu)
print(centroidlinkage.chechhaecht.min.index)

centroidlinkage.size.chechhaecht.min <- length(which(centroidlinkage.chechhaecht.Eu==centroidlinkage.chechhaecht.Eu[which.min(centroidlinkage.chechhaecht.Eu)],arr.ind=T))
print(centroidlinkage.size.chechhaecht.min)

qqadddat = qqadddat+centroidlinkage.Eu[1]
qqaddmar = qqaddmar+centroidlinkage.Eu[2]
qqaddhee = qqaddhee+centroidlinkage.Eu[3]
qqaddhae = qqaddhae+centroidlinkage.Eu[4]
qqaddfru = qqaddfru+centroidlinkage.Eu[5]
qqaddcoo = qqaddcoo+centroidlinkage.Eu[6]

}



round(qqadddat/length(qqindex),3)
round(qqaddmar/length(qqindex),3)
round(qqaddhee/length(qqindex),3)
round(qqaddhae/length(qqindex),3)
round(qqaddfru/length(qqindex),3)
round(qqaddcoo/length(qqindex),3)

