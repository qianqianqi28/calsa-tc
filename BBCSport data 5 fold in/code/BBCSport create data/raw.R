#For section 5.3 of the first paper: this code is to calculate accuracy about RAW


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


load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\created matrix\\rawdtmBBCsport.Rdata")
# rownames(training_dtm[[1]])
# rownames(testing_dtm[[1]])
dimdefi <- 1
loo.len  = 1:length(testing_dtm)


average.averagelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.centroidlinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.singlelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);
average.completelinkage.accuracy <- matrix(rep(0,length(testing_dtm)*dimdefi), nrow=length(testing_dtm), ncol=dimdefi);

dim(average.averagelinkage.accuracy)

for (n in loo.len){
  
  print(n)
  
  averagelinkage.accuracy <- rep(0,dimdefi)
  centroidlinkage.accuracy <- rep(0,dimdefi)
  singlelinkage.accuracy <- rep(0,dimdefi)
  completelinkage.accuracy <- rep(0,dimdefi)
  
  row.name.cm.num.train <- training_index[[n]]
  row.name.cm.num.text <- testing_index[[n]]
  
  
  td.mat_mfi_choose.train <- training_dtm[[n]]
  test_new_matrix.svd.ud <- testing_dtm[[n]]
  
  td.mat_mfi_choose.train.datheen <- as.matrix(as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="1",]))
  td.mat_mfi_choose.train.marnix <- as.matrix(as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="2",]))
  td.mat_mfi_choose.train.heere <- as.matrix(as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="3",]))
  td.mat_mfi_choose.train.haecht <- as.matrix(as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="4",]))
  td.mat_mfi_choose.train.fruytiers <- as.matrix(as.matrix(td.mat_mfi_choose.train[row.name.cm.num.train=="5",]))
  
  
  for (k in 1:1){
    
    for(i in (1:nrow(test_new_matrix.svd.ud))){
      
      averagelinkage.Eu = c(0,0,0,0,0);
      centroidlinkage.Eu = c(0,0,0,0,0);
      singlelinkage.Eu = c(0,0,0,0,0);
      completelinkage.Eu = c(0,0,0,0,0);

      test_new_matrix.svd.ud_one <- t(as.matrix(test_new_matrix.svd.ud[i,]))
      
      
      query_train.datheen <- rbind(test_new_matrix.svd.ud_one[1,], td.mat_mfi_choose.train.datheen)
      Eu_query_train.datheen <- dist(query_train.datheen,"euclidean",diag = 1, upper = 1)
      Eu_query_train.datheen <- as.matrix(Eu_query_train.datheen)
      
      query_train.marnix <- rbind(test_new_matrix.svd.ud_one[1,], td.mat_mfi_choose.train.marnix)
      Eu_query_train.marnix <- dist(query_train.marnix,"euclidean",diag = 1, upper = 1)
      Eu_query_train.marnix <- as.matrix(Eu_query_train.marnix)
      
      query_train.heere <- rbind(test_new_matrix.svd.ud_one[1,], td.mat_mfi_choose.train.heere)
      Eu_query_train.heere <- dist(query_train.heere,"euclidean",diag = 1, upper = 1)
      Eu_query_train.heere <- as.matrix(Eu_query_train.heere)
      
      query_train.haecht <- rbind(test_new_matrix.svd.ud_one[1,], td.mat_mfi_choose.train.haecht)
      Eu_query_train.haecht <- dist(query_train.haecht,"euclidean",diag = 1, upper = 1)
      Eu_query_train.haecht <- as.matrix(Eu_query_train.haecht)
      
      query_train.fruytiers <- rbind(test_new_matrix.svd.ud_one[1,], td.mat_mfi_choose.train.fruytiers)
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
      
      
      
      
      # averagelinkage.size.min[k,i] <- length(which(averagelinkage.Eu==averagelinkage.Eu[which.min(averagelinkage.Eu)],arr.ind=T))
      
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
      
      
      
      
      # singlelinkage.size.min[k,i] <- length(which(singlelinkage.Eu==singlelinkage.Eu[which.min(singlelinkage.Eu)],arr.ind=T))
      
      singlelinkage.min.index <- which.min(singlelinkage.Eu)
      
      if (singlelinkage.min.index == row.name.cm.num.text[i]) {
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
      
      
      
      # completelinkage.size.min[k,i] <- length(which(completelinkage.Eu==completelinkage.Eu[which.min(completelinkage.Eu)],arr.ind=T))
      
      completelinkage.min.index <- which.min(completelinkage.Eu)
      
      if (completelinkage.min.index == row.name.cm.num.text[i]) {
        completelinkage.accuracy[k] <- completelinkage.accuracy[k]+1
      }
      
      #centroid linkage
      centroidlinkage.td.mat_mfi_choose.train.datheen <- t(as.matrix(apply(td.mat_mfi_choose.train.datheen,2,mean)))
      centroidlinkage.td.mat_mfi_choose.train.marnix <- t(as.matrix(apply(td.mat_mfi_choose.train.marnix,2,mean)))
      centroidlinkage.td.mat_mfi_choose.train.heere <- t(as.matrix(apply(td.mat_mfi_choose.train.heere,2,mean)))
      centroidlinkage.td.mat_mfi_choose.train.haecht <- t(as.matrix(apply(td.mat_mfi_choose.train.haecht,2,mean)))
      centroidlinkage.td.mat_mfi_choose.train.fruytiers <- t(as.matrix(apply(td.mat_mfi_choose.train.fruytiers,2,mean)))
      
      
      centroidlinkage_Eu_query_train.datheen <- sqrt(sum((test_new_matrix.svd.ud_one-centroidlinkage.td.mat_mfi_choose.train.datheen)^2))
      centroidlinkage_Eu_query_train.marnix <- sqrt(sum((test_new_matrix.svd.ud_one-centroidlinkage.td.mat_mfi_choose.train.marnix)^2))
      centroidlinkage_Eu_query_train.heere <- sqrt(sum((test_new_matrix.svd.ud_one-centroidlinkage.td.mat_mfi_choose.train.heere)^2))
      centroidlinkage_Eu_query_train.haecht <- sqrt(sum((test_new_matrix.svd.ud_one-centroidlinkage.td.mat_mfi_choose.train.haecht)^2))
      centroidlinkage_Eu_query_train.fruytiers <- sqrt(sum((test_new_matrix.svd.ud_one-centroidlinkage.td.mat_mfi_choose.train.fruytiers)^2))
      centroidlinkage.Eu[1] <- centroidlinkage_Eu_query_train.datheen
      centroidlinkage.Eu[2] <- centroidlinkage_Eu_query_train.marnix
      centroidlinkage.Eu[3] <- centroidlinkage_Eu_query_train.heere
      centroidlinkage.Eu[4] <- centroidlinkage_Eu_query_train.haecht
      centroidlinkage.Eu[5] <- centroidlinkage_Eu_query_train.fruytiers
      
      # centroidlinkage.size.min[k,i] <- length(which(centroidlinkage.Eu==centroidlinkage.Eu[which.min(centroidlinkage.Eu)],arr.ind=T))
      centroidlinkage.min.index <- which.min(centroidlinkage.Eu)
      
      if (centroidlinkage.min.index == row.name.cm.num.text[i]) {
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

rawaveragelinkage <- averagelinkage_kfold
write.csv(rawaveragelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\average linkage\\rawaveragelinkage.csv")
rawaveragelinkage
# rawaverage.averagelinkage.accuracy <- average.averagelinkage.accuracy
# write.csv(rawaverage.averagelinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\average linkage\\rawaverage.averagelinkage.accuracy.csv")

rawsinglelinkage <- singlelinkage_kfold
write.csv(rawsinglelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\rawsinglelinkage.csv")
rawsinglelinkage
# rawaverage.singlelinkage.accuracy <- average.singlelinkage.accuracy
# write.csv(rawaverage.singlelinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\rawaverage.singlelinkage.accuracy.csv")


rawcompletelinkage <- completelinkage_kfold
write.csv(rawcompletelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\rawcompletelinkage.csv")
rawcompletelinkage
# rawaverage.completelinkage.accuracy <- average.completelinkage.accuracy
# write.csv(rawaverage.completelinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\rawaverage.completelinkage.accuracy.csv")

rawcentroidlinkage <- centroidlinkage_kfold
write.csv(rawcentroidlinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\centroid linkage\\rawcentroidlinkage.csv")
rawcentroidlinkage
# rawaverage.centroidlinkage.accuracy <- average.centroidlinkage.accuracy
# write.csv(rawaverage.centroidlinkage.accuracy, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\centroid linkage\\rawaverage.centroidlinkage.accuracy.csv")

# dimdefi*nrow(test_new_matrix.svd.ud)
# sum(averagelinkage.size.min)
# sum(singlelinkage.size.min)
# sum(completelinkage.size.min)
# sum(centroidlinkage.size.min)
timeend<-Sys.time()
timeend- timestart
