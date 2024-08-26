#For section 5.3 of the first paper: this code is to calculate accuracy about LSA-RAW


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


dimdefi <- min(450, rankMatrix(training_dtm))




row.name.cm.num.train <- training_index
row.name.cm.num.text <- testing_index

dtm.training.lsaraw.svd <- svd(training_dtm)

td.mat_mfi_choose.train <- as.matrix(dtm.training.lsaraw.svd$u)%*%as.matrix(diag(dtm.training.lsaraw.svd$d))
test_new_matrix.svd.ud <- as.matrix(testing_dtm) %*% as.matrix(dtm.training.lsaraw.svd$v)



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


lsarawaveragelinkage <- average.averagelinkage.accuracy
write.csv(lsarawaveragelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\average linkage\\lsarawaveragelinkage.csv")

lsarawsinglelinkage <- average.singlelinkage.accuracy
write.csv(lsarawsinglelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\lsarawsinglelinkage.csv")

lsarawcompletelinkage <- average.completelinkage.accuracy
write.csv(lsarawcompletelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\complete linkage\\lsarawcompletelinkage.csv")

lsarawcentroidlinkage <- average.centroidlinkage.accuracy
write.csv(lsarawcentroidlinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\lsarawcentroidlinkage.csv")

timeend<-Sys.time()
timeend- timestart
