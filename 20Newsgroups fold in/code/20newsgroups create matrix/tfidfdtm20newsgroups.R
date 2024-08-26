#For section 5.3 of the first paper: this code is for 2963 documents from three categories where 1779 documents are from training set and 1184 documents are from validation set. Note the documents from the training set form a tfidf document-term matrix and the documents of validation set are not included in the matrix.


rm(list=ls())
load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\created matrix\\rawdtm20newsgroups.Rdata")

dim(training_dtm)
dim(testing_dtm)


sum(training_index==1)
sum(training_index==2)
sum(training_index==3)
sum(training_index==4)
sum(training_index==5)

sum(testing_index==1)
sum(testing_index==2)
sum(testing_index==3)
sum(testing_index==4)
sum(testing_index==5)

#TF matrix
TF <- as.matrix(training_dtm) 
dim(TF)

#IDF matrix
pre.IDF <- training_dtm
pre.IDF[pre.IDF > 0.5] <- 1
IDF <- log(nrow(pre.IDF)/colSums(pre.IDF),base = 2)

#plus 1
IDF <- IDF +1
IDF <- diag(IDF)

#TF-IDF matrix
training_dtm <- TF %*% IDF
dim(training_dtm)
testing_dtm <- testing_dtm%*% IDF
dim(testing_dtm)

save(training_dtm, testing_dtm, training_index, testing_index, file = "C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\created matrix\\tfidfdtm20newsgroups.Rdata")


