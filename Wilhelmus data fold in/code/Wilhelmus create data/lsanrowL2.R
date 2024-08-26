#For section 6.3 of the first paper: this code is to calculate accuracy about LSA-NROWL2


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


load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\created matrix\\nrowL2dtmWilhelmusdataset.Rdata")

loo.len  = 1:length(testing_dtm)


dimdefi <- rankMatrix(training_dtm[[1]])
dimdefi


averagelinkage.accuracy <- rep(0,dimdefi)
centroidlinkage.accuracy <- rep(0,dimdefi)
singlelinkage.accuracy <- rep(0,dimdefi)
completelinkage.accuracy <- rep(0,dimdefi)

average.averagelinkage.accuracy <- rep(0,dimdefi)
average.centroidlinkage.accuracy <- rep(0,dimdefi)
average.singlelinkage.accuracy <- rep(0,dimdefi)
average.completelinkage.accuracy <- rep(0,dimdefi)

averagelinkage.size.min <- matrix(rep(0,dimdefi*length(loo.len)), nrow=dimdefi, ncol=length(loo.len));
singlelinkage.size.min <- matrix(rep(0,dimdefi*length(loo.len)), nrow=dimdefi, ncol=length(loo.len));
completelinkage.size.min <- matrix(rep(0,dimdefi*length(loo.len)), nrow=dimdefi, ncol=length(loo.len));
centroidlinkage.size.min <- matrix(rep(0,dimdefi*length(loo.len)), nrow=dimdefi, ncol=length(loo.len));

dtm.training.lsaraw.svd.list <- list()
for (n in 1:length(testing_dtm)) {
  dtm.training.lsaraw.svd.list[[n]] <- svd(training_dtm[[n]])
}

for (k in 1:dimdefi){
  print(k)

  for(i in loo.len){
    
    dtm.training.lsaraw.svd <- dtm.training.lsaraw.svd.list[[i]]
    
    td.mat_mfi_choose.svd.ud <- as.matrix((as.matrix(dtm.training.lsaraw.svd$u)%*%as.matrix(diag(dtm.training.lsaraw.svd$d)))[,1:k])
    test_new_matrix.svd.ud <- t(as.matrix((as.matrix(testing_dtm[[i]]) %*% as.matrix(dtm.training.lsaraw.svd$v))[,1:k]))
    
    
    averagelinkage.Eu = c(0,0,0,0,0,0);
    centroidlinkage.Eu = c(0,0,0,0,0,0);
    singlelinkage.Eu = c(0,0,0,0,0,0);
    completelinkage.Eu = c(0,0,0,0,0,0);
    
    
    row.name.cm.num.train <- training_index[[i]]
    row.name.cm.num.text <- testing_index[[i]]
    
    td.mat_mfi_choose.train <- training_dtm[[i]]
    test_new_matrix <- testing_dtm[[i]]
    
    
    td.mat_mfi_choose.train.datheen <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="datheen",])
    td.mat_mfi_choose.train.marnix <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="marnix",])
    td.mat_mfi_choose.train.heere <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="heere",])
    td.mat_mfi_choose.train.haecht <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="haecht",])
    td.mat_mfi_choose.train.fruytiers <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="fruytiers",])
    td.mat_mfi_choose.train.coornhert <- as.matrix(td.mat_mfi_choose.svd.ud[rownames(td.mat_mfi_choose.train)=="coornhert",])
    
    
    query_train.datheen <- rbind(test_new_matrix.svd.ud[1,], td.mat_mfi_choose.train.datheen)
    Eu_query_train.datheen <- dist(query_train.datheen,"euclidean",diag = 1, upper = 1)
    Eu_query_train.datheen <- as.matrix(Eu_query_train.datheen)
    
    query_train.marnix <- rbind(test_new_matrix.svd.ud[1,], td.mat_mfi_choose.train.marnix)
    Eu_query_train.marnix <- dist(query_train.marnix,"euclidean",diag = 1, upper = 1)
    Eu_query_train.marnix <- as.matrix(Eu_query_train.marnix)
    
    query_train.heere <- rbind(test_new_matrix.svd.ud[1,], td.mat_mfi_choose.train.heere)
    Eu_query_train.heere <- dist(query_train.heere,"euclidean",diag = 1, upper = 1)
    Eu_query_train.heere <- as.matrix(Eu_query_train.heere)
    
    query_train.haecht <- rbind(test_new_matrix.svd.ud[1,], td.mat_mfi_choose.train.haecht)
    Eu_query_train.haecht <- dist(query_train.haecht,"euclidean",diag = 1, upper = 1)
    Eu_query_train.haecht <- as.matrix(Eu_query_train.haecht)
    
    query_train.fruytiers <- rbind(test_new_matrix.svd.ud[1,], td.mat_mfi_choose.train.fruytiers)
    Eu_query_train.fruytiers <- dist(query_train.fruytiers,"euclidean",diag = 1, upper = 1)
    Eu_query_train.fruytiers <- as.matrix(Eu_query_train.fruytiers)
    
    query_train.coornhert <- rbind(test_new_matrix.svd.ud[1,], td.mat_mfi_choose.train.coornhert)
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
    
    averagelinkage.size.min[k,i] <- length(which(averagelinkage.Eu==averagelinkage.Eu[which.min(averagelinkage.Eu)],arr.ind=T))
    
    averagelinkage.min.index <- which.min(averagelinkage.Eu)
    
    if (averagelinkage.min.index == row.name.cm.num.text) {
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
    
    
    singlelinkage_Eu_query_train.coornhert <- min(Eu_query_train.coornhert[1,2:ncol(Eu_query_train.coornhert)])
    singlelinkage.Eu[6] <- singlelinkage_Eu_query_train.coornhert
    
    singlelinkage.size.min[k,i] <- length(which(singlelinkage.Eu==singlelinkage.Eu[which.min(singlelinkage.Eu)],arr.ind=T))
    
    singlelinkage.min.index <- which.min(singlelinkage.Eu)
    
    if (singlelinkage.min.index == row.name.cm.num.text) {
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
    
    
    completelinkage_Eu_query_train.coornhert <- max(Eu_query_train.coornhert[1,2:ncol(Eu_query_train.coornhert)])
    completelinkage.Eu[6] <- completelinkage_Eu_query_train.coornhert
    
    completelinkage.size.min[k,i] <- length(which(completelinkage.Eu==completelinkage.Eu[which.min(completelinkage.Eu)],arr.ind=T))
    
    completelinkage.min.index <- which.min(completelinkage.Eu)
    
    if (completelinkage.min.index == row.name.cm.num.text) {
      completelinkage.accuracy[k] <- completelinkage.accuracy[k]+1
    }
    
    #centroid linkage
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
    centroidlinkage.Eu[1] <- centroidlinkage_Eu_query_train.datheen
    centroidlinkage.Eu[2] <- centroidlinkage_Eu_query_train.marnix
    centroidlinkage.Eu[3] <- centroidlinkage_Eu_query_train.heere
    centroidlinkage.Eu[4] <- centroidlinkage_Eu_query_train.haecht
    centroidlinkage.Eu[5] <- centroidlinkage_Eu_query_train.fruytiers
    centroidlinkage.Eu[6] <- centroidlinkage_Eu_query_train.coornhert
    
    centroidlinkage.size.min[k,i] <- length(which(centroidlinkage.Eu==centroidlinkage.Eu[which.min(centroidlinkage.Eu)],arr.ind=T))
    centroidlinkage.min.index <- which.min(centroidlinkage.Eu)
    if (centroidlinkage.min.index == row.name.cm.num.text) {
      centroidlinkage.accuracy[k] <- centroidlinkage.accuracy[k]+1
    }
  }
  average.averagelinkage.accuracy[k] <- averagelinkage.accuracy[k]/length(testing_dtm)
  average.singlelinkage.accuracy[k] <- singlelinkage.accuracy[k]/length(testing_dtm)
  average.completelinkage.accuracy[k] <- completelinkage.accuracy[k]/length(testing_dtm)
  average.centroidlinkage.accuracy[k] <- centroidlinkage.accuracy[k]/length(testing_dtm)
}

lsanrowl2averagelinkage <- average.averagelinkage.accuracy
write.csv(lsanrowl2averagelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\lsanrowl2averagelinkage.csv")

lsanrowl2singlelinkage <- average.singlelinkage.accuracy
write.csv(lsanrowl2singlelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\single linkage\\lsanrowl2singlelinkage.csv")



lsanrowl2completelinkage <- average.completelinkage.accuracy
write.csv(lsanrowl2completelinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\complete linkage\\lsanrowl2completelinkage.csv")


lsanrowl2centroidlinkage <- average.centroidlinkage.accuracy
write.csv(lsanrowl2centroidlinkage, file ="C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\centroid linkage\\lsanrowl2centroidlinkage.csv")

dimdefi*length(loo.len)
sum(averagelinkage.size.min)
sum(singlelinkage.size.min)
sum(completelinkage.size.min)
sum(centroidlinkage.size.min)
