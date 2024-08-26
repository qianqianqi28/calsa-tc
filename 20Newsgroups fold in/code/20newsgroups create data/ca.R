#For section 5.3 of the first paper: this code is to calculate accuracy about CA


rm(list=ls())

timestart<-Sys.time()

library(caret)
library(e1071)
library(stringr)
library(tm)
library("readxl")
library("superml")
library("udpipe")
library("data.table")
library(Matrix)


load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\created matrix\\rawdtm20newsgroups.Rdata")


row.name.cm.num.train <- training_index
row.name.cm.num.text <- testing_index



training_dtm.P    <- training_dtm/sum(training_dtm)
### Row and column masses
training_dtm.r    <- apply(training_dtm.P, 1, sum)
training_dtm.c    <- apply(training_dtm.P, 2, sum)
### CA Step 1: the matrix S
training_dtm.Dr   <- diag(training_dtm.r)
training_dtm.Dc   <- diag(training_dtm.c)
training_dtm.Drmh <- diag(1/sqrt(training_dtm.r))
training_dtm.Dcmh <- diag(1/sqrt(training_dtm.c))

training_dtm.SR   <- training_dtm.Drmh%*%(training_dtm.P-training_dtm.r%o%training_dtm.c)%*%training_dtm.Dcmh
dtm.training.caraw.svd <- svd(training_dtm.SR)

training_dtm.csc <- training_dtm.Dcmh%*%dtm.training.caraw.svd$v

#CARAW
td.mat_mfi_choose.train <- as.matrix(training_dtm.Drmh%*%as.matrix(dtm.training.caraw.svd$u)%*%as.matrix(diag((dtm.training.caraw.svd$d))))

temp <- testing_dtm/matrix(rep(apply(testing_dtm,1,sum),each = ncol(testing_dtm)), ncol = ncol(testing_dtm), by = TRUE)

test_new_matrix.svd.ud <- as.matrix(temp) %*%as.matrix(training_dtm.csc)


dimdefi <- min(450, rankMatrix(training_dtm.SR))

td.mat_mfi_choose.train.datheen <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="1",])
td.mat_mfi_choose.train.marnix <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="2",])
td.mat_mfi_choose.train.heere <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="3",])

#centroid linkage
centroidlinkage.td.mat_mfi_choose.train.datheen <- t(as.matrix(apply(td.mat_mfi_choose.train.datheen,2,mean)))
centroidlinkage.td.mat_mfi_choose.train.marnix <- t(as.matrix(apply(td.mat_mfi_choose.train.marnix,2,mean)))
centroidlinkage.td.mat_mfi_choose.train.heere <- t(as.matrix(apply(td.mat_mfi_choose.train.heere,2,mean)))

Eu_test_train_datheen <- list()
Eu_test_train_marnix <- list()
Eu_test_train_heere <- list()
centroidlinkage_Eu_test_train_datheen <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));
centroidlinkage_Eu_test_train_marnix <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));
centroidlinkage_Eu_test_train_heere <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));

k <- 1

Eu_test_train_datheen[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.datheen[, 1:k]),1,function(y) apply(as.matrix(test_new_matrix.svd.ud[, 1:k]),1,function(x,y)dist(rbind(x,y)),y))
Eu_test_train_marnix[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.marnix[, 1:k]),1,function(y) apply(as.matrix(test_new_matrix.svd.ud[, 1:k]),1,function(x,y)dist(rbind(x,y)),y))
Eu_test_train_heere[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.heere[, 1:k]),1,function(y) apply(as.matrix(test_new_matrix.svd.ud[, 1:k]),1,function(x,y)dist(rbind(x,y)),y))
centroidlinkage_Eu_test_train_datheen[k,] <- apply(as.matrix(test_new_matrix.svd.ud[,1:k]),1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.datheen[1,1:k])))))
centroidlinkage_Eu_test_train_marnix[k,] <- apply(as.matrix(test_new_matrix.svd.ud[,1:k]),1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.marnix[1,1:k])))))
centroidlinkage_Eu_test_train_heere[k,] <- apply(as.matrix(test_new_matrix.svd.ud[,1:k]),1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.heere[1,1:k])))))

for (k in 2:dimdefi){
#for (k in 449:dimdefi){
  print(k)
  Eu_test_train_datheen[[k]] <- apply(td.mat_mfi_choose.train.datheen[, 1:k],1,function(y) apply(test_new_matrix.svd.ud[, 1:k],1,function(x,y)dist(rbind(x,y)),y))
  Eu_test_train_marnix[[k]] <- apply(td.mat_mfi_choose.train.marnix[, 1:k],1,function(y) apply(test_new_matrix.svd.ud[, 1:k],1,function(x,y)dist(rbind(x,y)),y))
  Eu_test_train_heere[[k]] <- apply(td.mat_mfi_choose.train.heere[, 1:k],1,function(y) apply(test_new_matrix.svd.ud[, 1:k],1,function(x,y)dist(rbind(x,y)),y))
  centroidlinkage_Eu_test_train_datheen[k,] <- apply(test_new_matrix.svd.ud[,1:k],1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.datheen[1,1:k])))))
  centroidlinkage_Eu_test_train_marnix[k,] <- apply(test_new_matrix.svd.ud[,1:k],1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.marnix[1,1:k])))))
  centroidlinkage_Eu_test_train_heere[k,] <- apply(test_new_matrix.svd.ud[,1:k],1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.heere[1,1:k])))))
}


average.averagelinkage.accuracy <- matrix(rep(0,1*dimdefi), nrow=1, ncol=dimdefi);
average.centroidlinkage.accuracy <- matrix(rep(0,1*dimdefi), nrow=1, ncol=dimdefi);
average.singlelinkage.accuracy <- matrix(rep(0,1*dimdefi), nrow=1, ncol=dimdefi);
average.completelinkage.accuracy <- matrix(rep(0,1*dimdefi), nrow=1, ncol=dimdefi);



averagelinkage.accuracy <- rep(0,dimdefi)
centroidlinkage.accuracy <- rep(0,dimdefi)
singlelinkage.accuracy <- rep(0,dimdefi)
completelinkage.accuracy <- rep(0,dimdefi)


for (k in 1:dimdefi){
#for (k in 449:dimdefi){
  print(k)
  
  
  for(i in (1:nrow(test_new_matrix.svd.ud))){
    # print(i)
    averagelinkage.Eu = c(0,0,0);
    centroidlinkage.Eu = c(0,0,0);
    singlelinkage.Eu = c(0,0,0);
    completelinkage.Eu = c(0,0,0);
    
    
    
    
    #average linkage
    averagelinkage.Eu[1] <- (sum(Eu_test_train_datheen[[k]][i,]))/(ncol(Eu_test_train_datheen[[k]]))
    
    
    averagelinkage.Eu[2] <- (sum(Eu_test_train_marnix[[k]][i,]))/(ncol(Eu_test_train_marnix[[k]]))
    
    
    averagelinkage.Eu[3] <- (sum(Eu_test_train_heere[[k]][i,]))/(ncol(Eu_test_train_heere[[k]]))
    
    
    averagelinkage.min.index <- which.min(averagelinkage.Eu)
    
    if (averagelinkage.min.index == row.name.cm.num.text[i]) {
      averagelinkage.accuracy[k] <- averagelinkage.accuracy[k]+1
    }
    
    #single linkage
    singlelinkage_Eu_query_train.datheen <- min(Eu_test_train_datheen[[k]][i,])
    singlelinkage.Eu[1] <- singlelinkage_Eu_query_train.datheen
    
    
    singlelinkage_Eu_query_train.marnix <- min(Eu_test_train_marnix[[k]][i,])
    singlelinkage.Eu[2] <- singlelinkage_Eu_query_train.marnix
    
    
    
    singlelinkage_Eu_query_train.heere <- min(Eu_test_train_heere[[k]][i,])
    singlelinkage.Eu[3] <- singlelinkage_Eu_query_train.heere
    
    
    singlelinkage.min.index <- which.min(singlelinkage.Eu)
    
    if (singlelinkage.min.index ==  row.name.cm.num.text[i]) {
      singlelinkage.accuracy[k] <- singlelinkage.accuracy[k]+1
    }
    
    
    #complete linkage
    completelinkage.Eu[1] <- max(Eu_test_train_datheen[[k]][i,])
    
    completelinkage.Eu[2] <- max(Eu_test_train_marnix[[k]][i,])
    
    
    completelinkage.Eu[3] <- max(Eu_test_train_heere[[k]][i,])
    
    
    completelinkage.min.index <- which.min(completelinkage.Eu)
    
    if (completelinkage.min.index ==  row.name.cm.num.text[i]) {
      completelinkage.accuracy[k] <- completelinkage.accuracy[k]+1
    }
    
    
    #centroid
    centroidlinkage.Eu[1] <- centroidlinkage_Eu_test_train_datheen[k,i]
    centroidlinkage.Eu[2] <- centroidlinkage_Eu_test_train_marnix[k,i]
    centroidlinkage.Eu[3] <- centroidlinkage_Eu_test_train_heere[k,i]
    
    
    centroidlinkage.min.index <- which.min(centroidlinkage.Eu)
    
    if (centroidlinkage.min.index ==  row.name.cm.num.text[i]) {
      centroidlinkage.accuracy[k] <- centroidlinkage.accuracy[k]+1
    }
    
    
  }
  average.averagelinkage.accuracy[1, k] <- averagelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
  average.singlelinkage.accuracy[1, k] <- singlelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
  average.completelinkage.accuracy[1, k] <- completelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
  average.centroidlinkage.accuracy[1, k] <- centroidlinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
  print(average.averagelinkage.accuracy[1, k])
  print(average.singlelinkage.accuracy[1, k])
  print(average.completelinkage.accuracy[1, k])
  print(average.centroidlinkage.accuracy[1, k])
}

carawaveragelinkage <- average.averagelinkage.accuracy
write.csv(carawaveragelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\average linkage\\carawaveragelinkage.csv")

carawsinglelinkage <- average.singlelinkage.accuracy
write.csv(carawsinglelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\carawsinglelinkage.csv")

carawcompletelinkage <- average.completelinkage.accuracy
write.csv(carawcompletelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\complete linkage\\carawcompletelinkage.csv")

carawcentroidlinkage <- average.centroidlinkage.accuracy
write.csv(carawcentroidlinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\carawcentroidlinkage.csv")

timeend<-Sys.time()
timeend- timestart
