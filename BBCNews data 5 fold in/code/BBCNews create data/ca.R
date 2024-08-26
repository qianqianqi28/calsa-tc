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

load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\created matrix\\rawdtmBBCnews.Rdata")

# rownames(training_dtm[[1]])
# rownames(testing_dtm[[1]])
loo.len  = 1:length(testing_dtm)


training_dtm.Drmh <- list()
training_dtm.Dcmh <- list()
dtm.training.caraw.svd.list <- list()


dimdefi <- 450


for (n in 1:length(testing_dtm)){
  training_dtm.P    <- training_dtm[[n]]/sum(training_dtm[[n]])
  ### Row and column masses
  training_dtm.r    <- apply(training_dtm.P, 1, sum)
  training_dtm.c    <- apply(training_dtm.P, 2, sum)
  ### CA Step 1: the matrix S
  training_dtm.Dr   <- diag(training_dtm.r)
  training_dtm.Dc   <- diag(training_dtm.c)
  training_dtm.Drmh[[n]] <- diag(1/sqrt(training_dtm.r))
  training_dtm.Dcmh[[n]] <- diag(1/sqrt(training_dtm.c))
  
  training_dtm.SR   <- training_dtm.Drmh[[n]]%*%(training_dtm.P-training_dtm.r%o%training_dtm.c)%*%training_dtm.Dcmh[[n]]
  dtm.training.caraw.svd.list[[n]] <- svd(training_dtm.SR)
  dimdefi <- min(dimdefi, rankMatrix(training_dtm.SR))
}

average.averagelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.centroidlinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.singlelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.completelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);


for (n in loo.len){
  
  print(n)
  
  averagelinkage.accuracy <- rep(0,dimdefi)
  centroidlinkage.accuracy <- rep(0,dimdefi)
  singlelinkage.accuracy <- rep(0,dimdefi)
  completelinkage.accuracy <- rep(0,dimdefi)
  
  
  row.name.cm.num.train <- training_index[[n]]
  row.name.cm.num.text <- testing_index[[n]]
  
  
  dtm.training.caraw.svd <- dtm.training.caraw.svd.list[[n]]
  training_dtm.csc <- training_dtm.Dcmh[[n]]%*%dtm.training.caraw.svd$v
  
  #CARAW
  td.mat_mfi_choose.train <- as.matrix(training_dtm.Drmh[[n]]%*%as.matrix(dtm.training.caraw.svd$u)%*%as.matrix(diag((dtm.training.caraw.svd$d))))
  
  temp <- testing_dtm[[n]]/matrix(rep(apply(testing_dtm[[n]],1,sum),each = ncol(testing_dtm[[n]])), ncol = ncol(testing_dtm[[n]]), by = TRUE)
  
  test_new_matrix.svd.ud <- as.matrix(temp) %*%as.matrix(training_dtm.csc)
  
  
  td.mat_mfi_choose.train.datheen <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="1",])
  td.mat_mfi_choose.train.marnix <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="2",])
  td.mat_mfi_choose.train.heere <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="3",])
  td.mat_mfi_choose.train.haecht <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="4",])
  td.mat_mfi_choose.train.fruytiers <- as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="5",])
  
  #centroid linkage
  centroidlinkage.td.mat_mfi_choose.train.datheen <- t(as.matrix(apply(td.mat_mfi_choose.train.datheen,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.marnix <- t(as.matrix(apply(td.mat_mfi_choose.train.marnix,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.heere <- t(as.matrix(apply(td.mat_mfi_choose.train.heere,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.haecht <- t(as.matrix(apply(td.mat_mfi_choose.train.haecht,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.fruytiers <- t(as.matrix(apply(td.mat_mfi_choose.train.fruytiers,2,mean)))
  
  
  
  Eu_test_train_datheen <- list()
  Eu_test_train_marnix <- list()
  Eu_test_train_heere <- list()
  Eu_test_train_haecht <- list()
  Eu_test_train_fruytiers <- list()
  
  centroidlinkage_Eu_test_train_datheen <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));
  centroidlinkage_Eu_test_train_marnix <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));
  centroidlinkage_Eu_test_train_heere <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));
  centroidlinkage_Eu_test_train_haecht <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));
  centroidlinkage_Eu_test_train_fruytiers <- matrix(rep(0,dimdefi*nrow(test_new_matrix.svd.ud)), nrow=dimdefi, ncol=nrow(test_new_matrix.svd.ud));
  
  
  
  
  
  
  for (k in 1:dimdefi){
    print(k)
    temp_test_new_matrix.svd.ud <- as.matrix(test_new_matrix.svd.ud[, 1:k])
    Eu_test_train_datheen[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.datheen[, 1:k]),1,function(y) apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,y)),y))
    Eu_test_train_marnix[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.marnix[, 1:k]),1,function(y) apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,y)),y))
    Eu_test_train_heere[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.heere[, 1:k]),1,function(y) apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,y)),y))
    Eu_test_train_haecht[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.haecht[, 1:k]),1,function(y) apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,y)),y))
    Eu_test_train_fruytiers[[k]] <- apply(as.matrix(td.mat_mfi_choose.train.fruytiers[, 1:k]),1,function(y) apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,y)),y))
    
    centroidlinkage_Eu_test_train_datheen[k,] <- apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.datheen[1,1:k])))))
    centroidlinkage_Eu_test_train_marnix[k,] <- apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.marnix[1,1:k])))))
    centroidlinkage_Eu_test_train_heere[k,] <- apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.heere[1,1:k])))))
    centroidlinkage_Eu_test_train_haecht[k,] <- apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.haecht[1,1:k])))))
    centroidlinkage_Eu_test_train_fruytiers[k,] <- apply(temp_test_new_matrix.svd.ud,1,function(x,y)dist(rbind(x,t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.fruytiers[1,1:k])))))
    
    
    for(i in (1:nrow(test_new_matrix.svd.ud))){
      averagelinkage.Eu = c(0,0,0,0,0);
      centroidlinkage.Eu = c(0,0,0,0,0);
      singlelinkage.Eu = c(0,0,0,0,0);
      completelinkage.Eu = c(0,0,0,0,0);
      
      
      
      #average linkage
      averagelinkage.Eu[1] <- (sum(Eu_test_train_datheen[[k]][i,]))/(ncol(Eu_test_train_datheen[[k]]))
      
      averagelinkage.Eu[2] <- (sum(Eu_test_train_marnix[[k]][i,]))/(ncol(Eu_test_train_marnix[[k]]))
      
      averagelinkage.Eu[3] <- (sum(Eu_test_train_heere[[k]][i,]))/(ncol(Eu_test_train_heere[[k]]))
      
      
      averagelinkage.Eu[4] <- (sum(Eu_test_train_haecht[[k]][i,]))/(ncol(Eu_test_train_haecht[[k]]))
      
      averagelinkage.Eu[5] <- (sum(Eu_test_train_fruytiers[[k]][i,]))/(ncol(Eu_test_train_fruytiers[[k]]))
      
      
      
      
      averagelinkage.min.index <- which.min(averagelinkage.Eu)
      
      if (averagelinkage.min.index == row.name.cm.num.text[i]) {
        averagelinkage.accuracy[k] <- averagelinkage.accuracy[k]+1
      }
      
      #single linkage
      singlelinkage.Eu[1] <- min(Eu_test_train_datheen[[k]][i,])
      
      
      singlelinkage.Eu[2] <- min(Eu_test_train_marnix[[k]][i,])
      
      
      
      singlelinkage.Eu[3] <- min(Eu_test_train_heere[[k]][i,])
      
      singlelinkage.Eu[4] <- min(Eu_test_train_haecht[[k]][i,])
      
      
      singlelinkage.Eu[5] <- min(Eu_test_train_fruytiers[[k]][i,])
      
      singlelinkage.min.index <- which.min(singlelinkage.Eu)
      
      if (singlelinkage.min.index ==  row.name.cm.num.text[i]) {
        singlelinkage.accuracy[k] <- singlelinkage.accuracy[k]+1
      }
      
      
      #complete linkage
      completelinkage.Eu[1] <- max(Eu_test_train_datheen[[k]][i,])
      
      completelinkage.Eu[2] <- max(Eu_test_train_marnix[[k]][i,])
      
      
      completelinkage.Eu[3] <- max(Eu_test_train_heere[[k]][i,])
      
      
      completelinkage.Eu[4] <- max(Eu_test_train_haecht[[k]][i,])
      
      
      
      completelinkage.Eu[5] <- max(Eu_test_train_fruytiers[[k]][i,])
      
      
      completelinkage.min.index <- which.min(completelinkage.Eu)
      
      if (completelinkage.min.index ==  row.name.cm.num.text[i]) {
        completelinkage.accuracy[k] <- completelinkage.accuracy[k]+1
      }
      
      
      #centroid
      centroidlinkage.Eu[1] <- centroidlinkage_Eu_test_train_datheen[k,i]
      centroidlinkage.Eu[2] <- centroidlinkage_Eu_test_train_marnix[k,i]
      centroidlinkage.Eu[3] <- centroidlinkage_Eu_test_train_heere[k,i]
      centroidlinkage.Eu[4] <- centroidlinkage_Eu_test_train_haecht[k,i]
      centroidlinkage.Eu[5] <- centroidlinkage_Eu_test_train_fruytiers[k,i]
      
      centroidlinkage.min.index <- which.min(centroidlinkage.Eu)
      
      if (centroidlinkage.min.index ==  row.name.cm.num.text[i]) {
        centroidlinkage.accuracy[k] <- centroidlinkage.accuracy[k]+1
      }
      
      
    }
    average.averagelinkage.accuracy[n, k] <- averagelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    average.singlelinkage.accuracy[n, k] <- singlelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    average.completelinkage.accuracy[n, k] <- completelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    average.centroidlinkage.accuracy[n, k] <- centroidlinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    print(average.averagelinkage.accuracy[n, k])
    print(average.singlelinkage.accuracy[n, k])
    print(average.completelinkage.accuracy[n, k])
    print(average.centroidlinkage.accuracy[n, k])
  }
  
  averagelinkage_kfold <- apply(average.averagelinkage.accuracy, 2, sum)/length(testing_dtm)
  singlelinkage_kfold <- apply(average.singlelinkage.accuracy, 2, sum)/length(testing_dtm)
  completelinkage_kfold <- apply(average.completelinkage.accuracy, 2, sum)/length(testing_dtm)
  centroidlinkage_kfold <- apply(average.centroidlinkage.accuracy, 2, sum)/length(testing_dtm)
}
carawaveragelinkage <- averagelinkage_kfold
write.csv(carawaveragelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\average linkage\\carawaveragelinkage.csv")


carawsinglelinkage <- singlelinkage_kfold
write.csv(carawsinglelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\single linkage\\carawsinglelinkage.csv")



carawcompletelinkage <- completelinkage_kfold
write.csv(carawcompletelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\complete linkage\\carawcompletelinkage.csv")


carawcentroidlinkage <- centroidlinkage_kfold
write.csv(carawcentroidlinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\centroid linkage\\carawcentroidlinkage.csv")



timeend<-Sys.time()
timeend- timestart
