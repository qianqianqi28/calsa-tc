#For section 5.3 of the first paper: this code is to calculate accuracy about single method


rm(list=ls())

rawprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\rawsinglelinkage.csv",as.is = TRUE)
rawlsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\lsarawsinglelinkage.csv",as.is = TRUE)
nrowl1lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\lsanrowl1singlelinkage.csv",as.is = TRUE)
nrowl2lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\lsanrowl2singlelinkage.csv",as.is = TRUE)

tfidflsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\lsatfidfsinglelinkage.csv",as.is = TRUE)
caprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\single linkage\\carawsinglelinkage.csv",as.is = TRUE)


rawprecionrecall <- rawprecionrecall[-1]
rawlsaprecionrecall <- rawlsaprecionrecall[-1]
nrowl1lsaprecionrecall <- nrowl1lsaprecionrecall[-1]
nrowl2lsaprecionrecall <- nrowl2lsaprecionrecall[-1]
tfidflsaprecionrecall <- tfidflsaprecionrecall[-1]
caprecionrecall <- caprecionrecall[-1]


t(which(caprecionrecall>as.numeric(rawprecionrecall[which.max(rawprecionrecall)]),arr.ind=T))
t(which(caprecionrecall>as.numeric(rawlsaprecionrecall[which.max(rawlsaprecionrecall)]),arr.ind=T))
t(which(caprecionrecall>as.numeric(nrowl1lsaprecionrecall[which.max(nrowl1lsaprecionrecall)]),arr.ind=T))
t(which(caprecionrecall>as.numeric(nrowl2lsaprecionrecall[which.max(nrowl2lsaprecionrecall)]),arr.ind=T))
t(which(caprecionrecall>as.numeric(tfidflsaprecionrecall[which.max(tfidflsaprecionrecall)]),arr.ind=T))

dimdefi <- length(rawlsaprecionrecall)
t(which(caprecionrecall[1:dimdefi]>as.numeric(rawprecionrecall),arr.ind=T))
t(which(caprecionrecall[1:dimdefi]>as.numeric(rawlsaprecionrecall[1:dimdefi]),arr.ind=T))
t(which(caprecionrecall[1:dimdefi]>as.numeric(nrowl1lsaprecionrecall[1:dimdefi]),arr.ind=T))
t(which(caprecionrecall[1:dimdefi]>as.numeric(nrowl2lsaprecionrecall[1:dimdefi]),arr.ind=T))
t(which(caprecionrecall[1:dimdefi]>as.numeric(tfidflsaprecionrecall[1:dimdefi]),arr.ind=T))



#Table 12: The optimal dimensionality k and the accuracy in k for LSARAW, LSANROW, LSATFIDF, and CA, and the accuracy for RAW using different distance measurement methods.
round(rawprecionrecall[1],3)

t(which(rawlsaprecionrecall==as.numeric(rawlsaprecionrecall[which.max(rawlsaprecionrecall)]),arr.ind=T))
round(rawlsaprecionrecall[which(rawlsaprecionrecall==as.numeric(rawlsaprecionrecall[which.max(rawlsaprecionrecall)]),arr.ind=T)],3)


t(which(nrowl1lsaprecionrecall==as.numeric(nrowl1lsaprecionrecall[which.max(nrowl1lsaprecionrecall)]),arr.ind=T))
round(nrowl1lsaprecionrecall[which(nrowl1lsaprecionrecall==as.numeric(nrowl1lsaprecionrecall[which.max(nrowl1lsaprecionrecall)]),arr.ind=T)],3)


t(which(nrowl2lsaprecionrecall==as.numeric(nrowl2lsaprecionrecall[which.max(nrowl2lsaprecionrecall)]),arr.ind=T))
round(nrowl2lsaprecionrecall[which(nrowl2lsaprecionrecall==as.numeric(nrowl2lsaprecionrecall[which.max(nrowl2lsaprecionrecall)]),arr.ind=T)],3)


t(which(tfidflsaprecionrecall==as.numeric(tfidflsaprecionrecall[which.max(tfidflsaprecionrecall)]),arr.ind=T))
round(tfidflsaprecionrecall[which(tfidflsaprecionrecall==as.numeric(tfidflsaprecionrecall[which.max(tfidflsaprecionrecall)]),arr.ind=T)],3)

t(which(caprecionrecall==as.numeric(caprecionrecall[which.max(caprecionrecall)]),arr.ind=T))
round(caprecionrecall[which(caprecionrecall==as.numeric(caprecionrecall[which.max(caprecionrecall)]),arr.ind=T)],3)

