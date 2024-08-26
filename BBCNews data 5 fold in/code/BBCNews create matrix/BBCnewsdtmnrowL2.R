#For section 5.3 of the first paper: this code is for 2225 documents from five categories using five-fold cross validation. Note each time the documents from the training set form a nrowl2 document-term matrix and the documents of validation set are not included in the matrix.


rm(list=ls())
timestart<-Sys.time()
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\created matrix\\rawdtmBBCnews.Rdata")

loo.len  = 1:length(testing_dtm)

for(i in loo.len){
  training_dtm[[i]] <- training_dtm[[i]]/matrix(rep(sqrt(apply(training_dtm[[i]]^2,1,sum)),each = ncol(training_dtm[[i]])), ncol = ncol(training_dtm[[i]]), by = TRUE)
  testing_dtm[[i]] <- testing_dtm[[i]]/matrix(rep(sqrt(apply(testing_dtm[[i]]^2,1,sum)),each = ncol(testing_dtm[[i]])), ncol = ncol(testing_dtm[[i]]), by = TRUE)
  apply(training_dtm[[i]]^2, 1, sum)
  apply(testing_dtm[[i]]^2, 1, sum)
}

save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\created data\\created matrix\\nrowL2dtmBBCnews.Rdata")
timeend<-Sys.time()
timeend- timestart
