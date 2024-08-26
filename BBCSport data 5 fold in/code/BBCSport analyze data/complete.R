#For section 5.3 of the first paper: this code is to calculate accuracy about complete method


rm(list=ls())

rawprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\rawcompletelinkage.csv",as.is = TRUE)
rawlsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\lsarawcompletelinkage.csv",as.is = TRUE)
nrowl1lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\lsanrowl1completelinkage.csv",as.is = TRUE)
nrowl2lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\lsanrowl2completelinkage.csv",as.is = TRUE)

tfidflsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\lsatfidfcompletelinkage.csv",as.is = TRUE)
caprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\complete linkage\\carawcompletelinkage.csv",as.is = TRUE)


which(caprecionrecall$x>rawprecionrecall$x[which.max(rawprecionrecall$x)],arr.ind=T)
which(caprecionrecall$x>rawlsaprecionrecall$x[which.max(rawlsaprecionrecall$x)],arr.ind=T)
which(caprecionrecall$x>nrowl1lsaprecionrecall$x[which.max(nrowl1lsaprecionrecall$x)],arr.ind=T)
which(caprecionrecall$x>nrowl2lsaprecionrecall$x[which.max(nrowl2lsaprecionrecall$x)],arr.ind=T)
which(caprecionrecall$x>tfidflsaprecionrecall$x[which.max(tfidflsaprecionrecall$x)],arr.ind=T)

dimdefi <- length(rawlsaprecionrecall$x)
which(caprecionrecall$x[1:dimdefi-1]>rawprecionrecall$x,arr.ind=T)
which(caprecionrecall$x[1:dimdefi-1]>rawlsaprecionrecall$x[1:dimdefi-1],arr.ind=T)
which(caprecionrecall$x[1:dimdefi-1]>nrowl1lsaprecionrecall$x[1:dimdefi-1],arr.ind=T)
which(caprecionrecall$x[1:dimdefi-1]>nrowl2lsaprecionrecall$x[1:dimdefi-1],arr.ind=T)
which(caprecionrecall$x[1:dimdefi-1]>tfidflsaprecionrecall$x[1:dimdefi-1],arr.ind=T)



#The optimal dimensionality k and the accuracy in k for LSARAW, LSANROWL1, LSANROWL2, LSATFIDF, and CA, and the accuracy for RAW using different distance measurement methods.
round(rawprecionrecall$x[1],3)

which(rawlsaprecionrecall$x==rawlsaprecionrecall$x[which.max(rawlsaprecionrecall$x)],arr.ind=T)
round(rawlsaprecionrecall$x[which(rawlsaprecionrecall$x==rawlsaprecionrecall$x[which.max(rawlsaprecionrecall$x)],arr.ind=T)],3)


which(nrowl1lsaprecionrecall$x==nrowl1lsaprecionrecall$x[which.max(nrowl1lsaprecionrecall$x)],arr.ind=T)
round(nrowl1lsaprecionrecall$x[which(nrowl1lsaprecionrecall$x==nrowl1lsaprecionrecall$x[which.max(nrowl1lsaprecionrecall$x)],arr.ind=T)],3)


which(nrowl2lsaprecionrecall$x==nrowl2lsaprecionrecall$x[which.max(nrowl2lsaprecionrecall$x)],arr.ind=T)
round(nrowl2lsaprecionrecall$x[which(nrowl2lsaprecionrecall$x==nrowl2lsaprecionrecall$x[which.max(nrowl2lsaprecionrecall$x)],arr.ind=T)],3)


which(tfidflsaprecionrecall$x==tfidflsaprecionrecall$x[which.max(tfidflsaprecionrecall$x)],arr.ind=T)
round(tfidflsaprecionrecall$x[which(tfidflsaprecionrecall$x==tfidflsaprecionrecall$x[which.max(tfidflsaprecionrecall$x)],arr.ind=T)],3)

which(caprecionrecall$x==caprecionrecall$x[which.max(caprecionrecall$x)],arr.ind=T)
round(caprecionrecall$x[which(caprecionrecall$x==caprecionrecall$x[which.max(caprecionrecall$x)],arr.ind=T)],3)

