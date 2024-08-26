#For section 6.3 of the first paper: this code is for songs from six authors using LOOCV. Note each time 185 documents from training set form a nrowl2 document-term matrix of size 185*300 and the single document of validation set is not included in the matrix.


rm(list=ls())
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\created matrix\\rawdtmWilhelmusdataset.Rdata")

loo.len  = 1:186

for(i in loo.len){
  training_dtm[[i]] <- training_dtm[[i]]/matrix(rep(sqrt(apply(training_dtm[[i]]^2,1,sum)),each = ncol(training_dtm[[i]])), ncol = ncol(training_dtm[[i]]), by = TRUE)
  testing_dtm[[i]] <- testing_dtm[[i]]/matrix(rep(sqrt(apply(testing_dtm[[i]]^2,1,sum)),each = ncol(testing_dtm[[i]])), ncol = ncol(testing_dtm[[i]]), by = TRUE)
  apply(training_dtm[[i]]^2, 1, sum)
  apply(testing_dtm[[i]]^2, 1, sum)
}

save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\created matrix\\nrowL2dtmWilhelmusdataset.Rdata")
