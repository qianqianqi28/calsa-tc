#For section 5.3 of the first paper: this code is for 2963 documents from three categories where 1779 documents are from training set and 1184 documents are from validation set. Note the documents from the training set form a nrowl1 document-term matrix and the documents of validation set are not included in the matrix.


rm(list=ls())
timestart<-Sys.time()
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\created matrix\\rawdtm20newsgroups.Rdata")
training_dtm <- training_dtm/matrix(rep(apply(training_dtm,1,sum),each = ncol(training_dtm)), ncol = ncol(training_dtm), by = TRUE)
apply(training_dtm,1,sum)
testing_dtm <- testing_dtm/matrix(rep(apply(testing_dtm,1,sum),each = ncol(testing_dtm)), ncol = ncol(testing_dtm), by = TRUE)
apply(testing_dtm,1,sum)
save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\created matrix\\nrowL1dtm20newsgroups.Rdata")
