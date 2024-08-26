#For section 6.3 of the first paper: this code is to calculate accuracy about average method


rm(list=ls())


rawprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\rawaveragelinkage.csv",as.is = TRUE)
rawlsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\lsarawaveragelinkage.csv",as.is = TRUE)
nrowl1lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\lsanrowl1averagelinkage.csv",as.is = TRUE)
nrowl2lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\lsanrowl2averagelinkage.csv",as.is = TRUE)
tfidflsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\lsatfidfaveragelinkage.csv",as.is = TRUE)
caprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\created data\\average linkage\\caaveragelinkage.csv",as.is = TRUE)


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

