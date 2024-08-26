#For section 5.3 of the first paper: this code is to calculate accuracy about LSA-TFIDF


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


load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\created matrix\\tfidfdtmBBCsport.Rdata")

# rownames(training_dtm[[1]])
# rownames(testing_dtm[[1]])
loo.len  = 1:length(testing_dtm)

dimdefi <- 1

for(n in loo.len){
  dimdefi <- max(dimdefi, rankMatrix(training_dtm[[n]]))
}




average.averagelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.centroidlinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.singlelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.completelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);


dtm.training.lsatfidf.svd.list <- list()
for (n in 1:length(testing_dtm)) {
  dtm.training.lsatfidf.svd.list[[n]] <- svd(training_dtm[[n]])
}

for (n in loo.len){
  
  print(n)
  
  averagelinkage.accuracy <- rep(0,dimdefi)
  centroidlinkage.accuracy <- rep(0,dimdefi)
  singlelinkage.accuracy <- rep(0,dimdefi)
  completelinkage.accuracy <- rep(0,dimdefi)
  
  
  row.name.cm.num.train <- training_index[[n]]
  row.name.cm.num.text <- testing_index[[n]]
  
  dtm.training.lsatfidf.svd <- dtm.training.lsatfidf.svd.list[[n]]
  
  td.mat_mfi_choose.train <- as.matrix(dtm.training.lsatfidf.svd$u)%*%as.matrix(diag(dtm.training.lsatfidf.svd$d))
  test_new_matrix.svd.ud <- as.matrix(testing_dtm[[n]]) %*% as.matrix(dtm.training.lsatfidf.svd$v)
  
  
  
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
  
  for (k in 1:dimdefi){
    print(k)
    
    
    for(i in (1:nrow(test_new_matrix.svd.ud))){
      averagelinkage.Eu = c(0,0,0,0,0);
      centroidlinkage.Eu = c(0,0,0,0,0);
      singlelinkage.Eu = c(0,0,0,0,0);
      completelinkage.Eu = c(0,0,0,0,0);
      
      
      
      test_new_matrix.svd.ud_one <- t(as.matrix(test_new_matrix.svd.ud[i,1:k]))
      
      
      
      
      
      query_train.datheen <- rbind(test_new_matrix.svd.ud_one, as.matrix(td.mat_mfi_choose.train.datheen[,1:k]))
      Eu_query_train.datheen <- dist(query_train.datheen,"euclidean",diag = 1, upper = 1)
      Eu_query_train.datheen <- as.matrix(Eu_query_train.datheen)
      
      query_train.marnix <- rbind(test_new_matrix.svd.ud_one, as.matrix(td.mat_mfi_choose.train.marnix[,1:k]))
      Eu_query_train.marnix <- dist(query_train.marnix,"euclidean",diag = 1, upper = 1)
      Eu_query_train.marnix <- as.matrix(Eu_query_train.marnix)
      
      query_train.heere <- rbind(test_new_matrix.svd.ud_one, as.matrix(td.mat_mfi_choose.train.heere[,1:k]))
      Eu_query_train.heere <- dist(query_train.heere,"euclidean",diag = 1, upper = 1)
      Eu_query_train.heere <- as.matrix(Eu_query_train.heere)
      
      query_train.haecht <- rbind(test_new_matrix.svd.ud_one, as.matrix(td.mat_mfi_choose.train.haecht[,1:k]))
      Eu_query_train.haecht <- dist(query_train.haecht,"euclidean",diag = 1, upper = 1)
      Eu_query_train.haecht <- as.matrix(Eu_query_train.haecht)
      
      query_train.fruytiers <- rbind(test_new_matrix.svd.ud_one, as.matrix(td.mat_mfi_choose.train.fruytiers[,1:k]))
      Eu_query_train.fruytiers <- dist(query_train.fruytiers,"euclidean",diag = 1, upper = 1)
      Eu_query_train.fruytiers <- as.matrix(Eu_query_train.fruytiers)
      
      
      
      #average linkage
      averagelinkage_Eu_query_train.datheen <- (sum(Eu_query_train.datheen[1,2:ncol(Eu_query_train.datheen)]))/(ncol(Eu_query_train.datheen)-1)
      averagelinkage.Eu[1] <- averagelinkage_Eu_query_train.datheen
      
      
      averagelinkage_Eu_query_train.marnix <- (sum(Eu_query_train.marnix[1,2:ncol(Eu_query_train.marnix)]))/(ncol(Eu_query_train.marnix)-1)
      averagelinkage.Eu[2] <- averagelinkage_Eu_query_train.marnix
      
      
      
      averagelinkage_Eu_query_train.heere <- (sum(Eu_query_train.heere[1,2:ncol(Eu_query_train.heere)]))/(ncol(Eu_query_train.heere)-1)
      averagelinkage.Eu[3] <- averagelinkage_Eu_query_train.heere
      
      
      
      averagelinkage_Eu_query_train.haecht <- (sum(Eu_query_train.haecht[1,2:ncol(Eu_query_train.haecht)]))/(ncol(Eu_query_train.haecht)-1)
      averagelinkage.Eu[4] <- averagelinkage_Eu_query_train.haecht
      
      
      
      averagelinkage_Eu_query_train.fruytiers <- (sum(Eu_query_train.fruytiers[1,2:ncol(Eu_query_train.fruytiers)]))/(ncol(Eu_query_train.fruytiers)-1)
      averagelinkage.Eu[5] <- averagelinkage_Eu_query_train.fruytiers
      
      
      
      averagelinkage.min.index <- which.min(averagelinkage.Eu)
      
      if (averagelinkage.min.index == row.name.cm.num.text[i]) {
        averagelinkage.accuracy[k] <- averagelinkage.accuracy[k]+1
      }
      
      #single linkage
      singlelinkage_Eu_query_train.datheen <- min(Eu_query_train.datheen[1,2:ncol(Eu_query_train.datheen)])
      singlelinkage.Eu[1] <- singlelinkage_Eu_query_train.datheen
      
      
      singlelinkage_Eu_query_train.marnix <- min(Eu_query_train.marnix[1,2:ncol(Eu_query_train.marnix)])
      singlelinkage.Eu[2] <- singlelinkage_Eu_query_train.marnix
      
      
      
      singlelinkage_Eu_query_train.heere <- min(Eu_query_train.heere[1,2:ncol(Eu_query_train.heere)])
      singlelinkage.Eu[3] <- singlelinkage_Eu_query_train.heere
      
      
      
      singlelinkage_Eu_query_train.haecht <- min(Eu_query_train.haecht[1,2:ncol(Eu_query_train.haecht)])
      singlelinkage.Eu[4] <- singlelinkage_Eu_query_train.haecht
      
      
      
      singlelinkage_Eu_query_train.fruytiers <- min(Eu_query_train.fruytiers[1,2:ncol(Eu_query_train.fruytiers)])
      singlelinkage.Eu[5] <- singlelinkage_Eu_query_train.fruytiers
      
      
      singlelinkage.min.index <- which.min(singlelinkage.Eu)
      
      if (singlelinkage.min.index ==  row.name.cm.num.text[i]) {
        singlelinkage.accuracy[k] <- singlelinkage.accuracy[k]+1
      }
      
      
      #complete linkage
      completelinkage_Eu_query_train.datheen <- max(Eu_query_train.datheen[1,2:ncol(Eu_query_train.datheen)])
      completelinkage.Eu[1] <- completelinkage_Eu_query_train.datheen
      
      
      completelinkage_Eu_query_train.marnix <- max(Eu_query_train.marnix[1,2:ncol(Eu_query_train.marnix)])
      completelinkage.Eu[2] <- completelinkage_Eu_query_train.marnix
      
      
      
      completelinkage_Eu_query_train.heere <- max(Eu_query_train.heere[1,2:ncol(Eu_query_train.heere)])
      completelinkage.Eu[3] <- completelinkage_Eu_query_train.heere
      
      
      
      completelinkage_Eu_query_train.haecht <- max(Eu_query_train.haecht[1,2:ncol(Eu_query_train.haecht)])
      completelinkage.Eu[4] <- completelinkage_Eu_query_train.haecht
      
      
      
      completelinkage_Eu_query_train.fruytiers <- max(Eu_query_train.fruytiers[1,2:ncol(Eu_query_train.fruytiers)])
      completelinkage.Eu[5] <- completelinkage_Eu_query_train.fruytiers
      
      
      completelinkage.min.index <- which.min(completelinkage.Eu)
      
      if (completelinkage.min.index ==  row.name.cm.num.text[i]) {
        completelinkage.accuracy[k] <- completelinkage.accuracy[k]+1
      }
      
      
      #centroid
      centroidlinkage_Eu_query_train.datheen <- sqrt(sum((test_new_matrix.svd.ud_one-t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.datheen[,1:k])))^2))
      centroidlinkage_Eu_query_train.marnix <- sqrt(sum((test_new_matrix.svd.ud_one-t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.marnix[,1:k])))^2))
      centroidlinkage_Eu_query_train.heere <- sqrt(sum((test_new_matrix.svd.ud_one-t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.heere[,1:k])))^2))
      centroidlinkage_Eu_query_train.haecht <- sqrt(sum((test_new_matrix.svd.ud_one-t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.haecht[,1:k])))^2))
      centroidlinkage_Eu_query_train.fruytiers <- sqrt(sum((test_new_matrix.svd.ud_one-t(as.matrix(centroidlinkage.td.mat_mfi_choose.train.fruytiers[,1:k])))^2))
      centroidlinkage.Eu[1] <- centroidlinkage_Eu_query_train.datheen
      centroidlinkage.Eu[2] <- centroidlinkage_Eu_query_train.marnix
      centroidlinkage.Eu[3] <- centroidlinkage_Eu_query_train.heere
      centroidlinkage.Eu[4] <- centroidlinkage_Eu_query_train.haecht
      centroidlinkage.Eu[5] <- centroidlinkage_Eu_query_train.fruytiers
      
      centroidlinkage.min.index <- which.min(centroidlinkage.Eu)
      
      if (centroidlinkage.min.index ==  row.name.cm.num.text[i]) {
        centroidlinkage.accuracy[k] <- centroidlinkage.accuracy[k]+1
      }
      
      
    }
    average.averagelinkage.accuracy[n, k] <- averagelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    average.singlelinkage.accuracy[n, k] <- singlelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    average.completelinkage.accuracy[n, k] <- completelinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    average.centroidlinkage.accuracy[n, k] <- centroidlinkage.accuracy[k]/nrow(test_new_matrix.svd.ud)
    
  }
  
  averagelinkage_kfold <- apply(average.averagelinkage.accuracy, 2, sum)/length(testing_dtm)
  singlelinkage_kfold <- apply(average.singlelinkage.accuracy, 2, sum)/length(testing_dtm)
  completelinkage_kfold <- apply(average.completelinkage.accuracy, 2, sum)/length(testing_dtm)
  centroidlinkage_kfold <- apply(average.centroidlinkage.accuracy, 2, sum)/length(testing_dtm)
}
lsatfidfaveragelinkage <- averagelinkage_kfold
write.csv(lsatfidfaveragelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\average linkage\\lsatfidfaveragelinkage.csv")
# lsatfidfaverage.averagelinkage.accuracy <- average.averagelinkage.accuracy
# write.csv(lsatfidfaverage.averagelinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\average linkage\\lsatfidfaverage.averagelinkage.accuracy.csv")
# 

lsatfidfsinglelinkage <- singlelinkage_kfold
write.csv(lsatfidfsinglelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\lsatfidfsinglelinkage.csv")

# lsatfidfaverage.singlelinkage.accuracy <- average.singlelinkage.accuracy
# write.csv(lsatfidfaverage.singlelinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\lsatfidfaverage.singlelinkage.accuracy.csv")



lsatfidfcompletelinkage <- completelinkage_kfold
write.csv(lsatfidfcompletelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\lsatfidfcompletelinkage.csv")
# lsatfidfaverage.completelinkage.accuracy <- average.completelinkage.accuracy
# write.csv(lsatfidfaverage.completelinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\lsatfidfaverage.completelinkage.accuracy.csv")


lsatfidfcentroidlinkage <- centroidlinkage_kfold
write.csv(lsatfidfcentroidlinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\centroid linkage\\lsatfidfcentroidlinkage.csv")
# lsatfidfaverage.centroidlinkage.accuracy <- average.centroidlinkage.accuracy
# write.csv(lsatfidfaverage.centroidlinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\centroid linkage\\lsatfidfaverage.centroidlinkage.accuracy.csv")

timeend<-Sys.time()
timeend- timestart

