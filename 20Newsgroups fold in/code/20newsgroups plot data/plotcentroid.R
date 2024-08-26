#For Figure 6(c) of section 5.3 of the first paper: this code is to calculate accuracy about centroid method


rm(list=ls())

rawprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\rawcentroidlinkage.csv",as.is = TRUE)
rawlsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\lsarawcentroidlinkage.csv",as.is = TRUE)
nrowl1lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\lsanrowl1centroidlinkage.csv",as.is = TRUE)
nrowl2lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\lsanrowl2centroidlinkage.csv",as.is = TRUE)

tfidflsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\lsatfidfcentroidlinkage.csv",as.is = TRUE)
caprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\created data\\centroid linkage\\carawcentroidlinkage.csv",as.is = TRUE)


rawprecionrecall <- rawprecionrecall[-1]
rawlsaprecionrecall <- rawlsaprecionrecall[-1]
nrowl1lsaprecionrecall <- nrowl1lsaprecionrecall[-1]
nrowl2lsaprecionrecall <- nrowl2lsaprecionrecall[-1]
tfidflsaprecionrecall <- tfidflsaprecionrecall[-1]
caprecionrecall <- caprecionrecall[-1]


#Accuracy versus the number of dimension for centroid method.
dimdefi <- length(rawlsaprecionrecall)
rawprecionrecallx <- rep(rawprecionrecall[1], times=dimdefi)



dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\figure\\20Newsgroups plot data\\F20Newsgroupsfoldincentroidfulldimension.eps")
par(mar=c(5,6,4,1)+.1)
plot(1:(dimdefi),caprecionrecall[1:dimdefi],col=1,type="l",ylim = c(0,1),
     lty=1,main = "20 Newsgroups (centroid method)", xlab = "Number of dimension", ylab = "Accuracy", lwd=1.8,cex.lab=1.75, cex.axis = 1.55, cex.main=1.75)
lines(1:dimdefi,rawlsaprecionrecall[1:dimdefi],col=2,lty=1, lwd=1.8)
lines(1:dimdefi,nrowl1lsaprecionrecall[1:dimdefi],col=3,lty=1, lwd=1.8)
lines(1:dimdefi,nrowl2lsaprecionrecall[1:dimdefi],col=4,lty=1, lwd=1.8)
lines(1:dimdefi,tfidflsaprecionrecall[1:dimdefi],col=5,lty=1, lwd=1.8)
lines(1:dimdefi,rawprecionrecallx,col=6,lty=1, lwd=1.8)
#title("centroid method",lwd=3)

legend("bottomright",cex=1,c("CA","LSA-RAW","LSA-NROWL1","LSA-NROWL2","LSA-TFIDF",
                             "RAW"),col=c(1,2,3,4,5,6),lty=c(1,1,1,1,1,1))

dev.off()


#Not used.
# dimdefi <- 10
# setEPS()
# postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\20Newsgroups fold in\\figure\\20Newsgroups plot data\\F20Newsgroupsfoldincentroidpartdimension.eps")
# par(mar=c(5,6,4,1)+.1)
# plot(1:(dimdefi),caprecionrecall[1:dimdefi],col=1,type="b",ylim = c(0,1),
#      lty=1,main = "20 Newsgroups (centroid method)", xlab = "Number of dimension", ylab = "Accuracy", lwd=1.8,cex.lab=1.75, cex.axis = 1.55, cex.main=1.75)
# lines(1:dimdefi,rawlsaprecionrecall[1:dimdefi],col=2,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,nrowl1lsaprecionrecall[1:dimdefi],col=3,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,nrowl2lsaprecionrecall[1:dimdefi],col=4,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,tfidflsaprecionrecall[1:dimdefi],col=5,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,rawprecionrecallx[1:dimdefi],col=6,type="b",lty=1, lwd=1.8)
# 
# 
# #title("centroid method",lwd=3)
# 
# legend("bottomright",cex=1,c("CA","LSA-RAW","LSA-NROWL1","LSA-NROWL2","LSA-TFIDF",
#                              "RAW"),col=c(1,2,3,4,5,6),lty=c(1,1,1,1,1,1),pch=c(1,1,1,1,1,1))
# 
# dev.off()

