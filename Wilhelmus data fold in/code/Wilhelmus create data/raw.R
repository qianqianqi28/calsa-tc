#For section 6.3 of the first paper: this code is to calculate accuracy about RAW


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

load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\created matrix\\rawdtmWilhelmusdataset.Rdata")
rownames(training_dtm[[1]])
rownames(testing_dtm[[1]])


loo.len  = 1:length(testing_dtm)



averagelinkage.accuracy <- c(0)
centroidlinkage.accuracy <- c(0)
singlelinkage.accuracy <- c(0)
completelinkage.accuracy <- c(0)

averagelinkage.size.min <- rep(0,length(loo.len))
singlelinkage.size.min <- rep(0,length(loo.len))
completelinkage.size.min <- rep(0,length(loo.len))
centroidlinkage.size.min <- rep(0,length(loo.len))



for(i in loo.len){
  
  averagelinkage.Eu = c(0,0,0,0,0,0);
  centroidlinkage.Eu = c(0,0,0,0,0,0);
  singlelinkage.Eu = c(0,0,0,0,0,0);
  completelinkage.Eu = c(0,0,0,0,0,0);
  
  
  row.name.cm.num.train <- training_index[[i]]
  row.name.cm.num.text <- testing_index[[i]]

  
  td.mat_mfi_choose.train <- training_dtm[[i]]
  test_new_matrix <- testing_dtm[[i]]
  
  td.mat_mfi_choose.train.datheen <- td.mat_mfi_choose.train[rownames(td.mat_mfi_choose.train)=="datheen",]
  td.mat_mfi_choose.train.marnix <- td.mat_mfi_choose.train[rownames(td.mat_mfi_choose.train)=="marnix",]
  td.mat_mfi_choose.train.heere <- td.mat_mfi_choose.train[rownames(td.mat_mfi_choose.train)=="heere",]
  td.mat_mfi_choose.train.haecht <- td.mat_mfi_choose.train[rownames(td.mat_mfi_choose.train)=="haecht",]
  td.mat_mfi_choose.train.fruytiers <- td.mat_mfi_choose.train[rownames(td.mat_mfi_choose.train)=="fruytiers",]
  td.mat_mfi_choose.train.coornhert <- td.mat_mfi_choose.train[rownames(td.mat_mfi_choose.train)=="coornhert",]
  
  
  
  query_train.datheen <- rbind(test_new_matrix[1,], td.mat_mfi_choose.train.datheen)
  Eu_query_train.datheen <- dist(query_train.datheen,"euclidean",diag = 1, upper = 1)
  Eu_query_train.datheen <- as.matrix(Eu_query_train.datheen)
  
  query_train.marnix <- rbind(test_new_matrix[1,], td.mat_mfi_choose.train.marnix)
  Eu_query_train.marnix <- dist(query_train.marnix,"euclidean",diag = 1, upper = 1)
  Eu_query_train.marnix <- as.matrix(Eu_query_train.marnix)
  
  query_train.heere <- rbind(test_new_matrix[1,], td.mat_mfi_choose.train.heere)
  Eu_query_train.heere <- dist(query_train.heere,"euclidean",diag = 1, upper = 1)
  Eu_query_train.heere <- as.matrix(Eu_query_train.heere)
  
  query_train.haecht <- rbind(test_new_matrix[1,], td.mat_mfi_choose.train.haecht)
  Eu_query_train.haecht <- dist(query_train.haecht,"euclidean",diag = 1, upper = 1)
  Eu_query_train.haecht <- as.matrix(Eu_query_train.haecht)
  
  query_train.fruytiers <- rbind(test_new_matrix[1,], td.mat_mfi_choose.train.fruytiers)
  Eu_query_train.fruytiers <- dist(query_train.fruytiers,"euclidean",diag = 1, upper = 1)
  Eu_query_train.fruytiers <- as.matrix(Eu_query_train.fruytiers)
  
  query_train.coornhert <- rbind(test_new_matrix[1,], td.mat_mfi_choose.train.coornhert)
  Eu_query_train.coornhert <- dist(query_train.coornhert,"euclidean",diag = 1, upper = 1)
  Eu_query_train.coornhert <- as.matrix(Eu_query_train.coornhert)
  
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
  
  
  averagelinkage_Eu_query_train.coornhert <- (sum(Eu_query_train.coornhert[1,2:ncol(Eu_query_train.coornhert)]))/(ncol(Eu_query_train.coornhert)-1)
  averagelinkage.Eu[6] <- averagelinkage_Eu_query_train.coornhert
  
  averagelinkage.size.min[i] <- length(which(averagelinkage.Eu==averagelinkage.Eu[which.min(averagelinkage.Eu)],arr.ind=T))
  
  averagelinkage.min.index <- which.min(averagelinkage.Eu)
  
  if (averagelinkage.min.index == row.name.cm.num.text) {
    averagelinkage.accuracy <- averagelinkage.accuracy+1
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
  
  
  singlelinkage_Eu_query_train.coornhert <- min(Eu_query_train.coornhert[1,2:ncol(Eu_query_train.coornhert)])
  singlelinkage.Eu[6] <- singlelinkage_Eu_query_train.coornhert
  
  singlelinkage.size.min[i] <- length(which(singlelinkage.Eu==singlelinkage.Eu[which.min(singlelinkage.Eu)],arr.ind=T))
  
  singlelinkage.min.index <- which.min(singlelinkage.Eu)
  
  if (singlelinkage.min.index == row.name.cm.num.text) {
    singlelinkage.accuracy <- singlelinkage.accuracy+1
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
  
  
  completelinkage_Eu_query_train.coornhert <- max(Eu_query_train.coornhert[1,2:ncol(Eu_query_train.coornhert)])
  completelinkage.Eu[6] <- completelinkage_Eu_query_train.coornhert
  
  completelinkage.size.min[i] <- length(which(completelinkage.Eu==completelinkage.Eu[which.min(completelinkage.Eu)],arr.ind=T))
  
  completelinkage.min.index <- which.min(completelinkage.Eu)
  
  if (completelinkage.min.index == row.name.cm.num.text) {
    completelinkage.accuracy <- completelinkage.accuracy+1
  }
  
  #centroid linkage
  centroidlinkage.td.mat_mfi_choose.train.datheen <- t(as.matrix(apply(td.mat_mfi_choose.train.datheen,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.marnix <- t(as.matrix(apply(td.mat_mfi_choose.train.marnix,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.heere <- t(as.matrix(apply(td.mat_mfi_choose.train.heere,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.haecht <- t(as.matrix(apply(td.mat_mfi_choose.train.haecht,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.fruytiers <- t(as.matrix(apply(td.mat_mfi_choose.train.fruytiers,2,mean)))
  centroidlinkage.td.mat_mfi_choose.train.coornhert <- t(as.matrix(apply(td.mat_mfi_choose.train.coornhert,2,mean)))
  
  
  centroidlinkage_Eu_query_train.datheen <- sqrt(sum((test_new_matrix-centroidlinkage.td.mat_mfi_choose.train.datheen)^2))
  centroidlinkage_Eu_query_train.marnix <- sqrt(sum((test_new_matrix-centroidlinkage.td.mat_mfi_choose.train.marnix)^2))
  centroidlinkage_Eu_query_train.heere <- sqrt(sum((test_new_matrix-centroidlinkage.td.mat_mfi_choose.train.heere)^2))
  centroidlinkage_Eu_query_train.haecht <- sqrt(sum((test_new_matrix-centroidlinkage.td.mat_mfi_choose.train.haecht)^2))
  centroidlinkage_Eu_query_train.fruytiers <- sqrt(sum((test_new_matrix-centroidlinkage.td.mat_mfi_choose.train.fruytiers)^2))
  centroidlinkage_Eu_query_train.coornhert <- sqrt(sum((test_new_matrix-centroidlinkage.td.mat_mfi_choose.train.coornhert)^2))
  centroidlinkage.Eu[1] <- centroidlinkage_Eu_query_train.datheen
  centroidlinkage.Eu[2] <- centroidlinkage_Eu_query_train.marnix
  centroidlinkage.Eu[3] <- centroidlinkage_Eu_query_train.heere
  centroidlinkage.Eu[4] <- centroidlinkage_Eu_query_train.haecht
  centroidlinkage.Eu[5] <- centroidlinkage_Eu_query_train.fruytiers
  centroidlinkage.Eu[6] <- centroidlinkage_Eu_query_train.coornhert
  
  centroidlinkage.size.min[i] <- length(which(centroidlinkage.Eu==centroidlinkage.Eu[which.min(centroidlinkage.Eu)],arr.ind=T))
  
  centroidlinkage.min.index <- which.min(centroidlinkage.Eu)
  if (centroidlinkage.min.index == row.name.cm.num.text) {
    centroidlinkage.accuracy <- centroidlinkage.accuracy+1
  }
  
} 
average.averagelinkage.accuracy <- averagelinkage.accuracy/length(testing_dtm)
average.singlelinkage.accuracy <- singlelinkage.accuracy/length(testing_dtm)
average.completelinkage.accuracy <- completelinkage.accuracy/length(testing_dtm)
average.centroidlinkage.accuracy <- centroidlinkage.accuracy/length(testing_dtm)
 

rawaveragelinkage <- average.averagelinkage.accuracy
write.csv(rawaveragelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\rawaveragelinkage.csv")

rawsinglelinkage <- average.singlelinkage.accuracy
write.csv(rawsinglelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\single linkage\\rawsinglelinkage.csv")

rawcompletelinkage <- average.completelinkage.accuracy
write.csv(rawcompletelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\complete linkage\\rawcompletelinkage.csv")


rawcentroidlinkage <- average.centroidlinkage.accuracy
write.csv(rawcentroidlinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\centroid linkage\\rawcentroidlinkage.csv")

length(loo.len)
sum(averagelinkage.size.min)
sum(singlelinkage.size.min)
sum(completelinkage.size.min)
sum(centroidlinkage.size.min)

