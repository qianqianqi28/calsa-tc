#For Figure 6(b) of section 5.3 of the first paper: this code is to calculate accuracy about single method


rm(list=ls())

rawprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\rawsinglelinkage.csv",as.is = TRUE)
rawlsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\lsarawsinglelinkage.csv",as.is = TRUE)
nrowl1lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\lsanrowl1singlelinkage.csv",as.is = TRUE)
nrowl2lsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\lsanrowl2singlelinkage.csv",as.is = TRUE)
tfidflsaprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\lsatfidfsinglelinkage.csv",as.is = TRUE)
caprecionrecall <- read.csv("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\created data\\single linkage\\carawsinglelinkage.csv",as.is = TRUE)



#Accuracy versus the number of dimension for single method.
dimdefi <- length(rawlsaprecionrecall$x)
rawprecionrecallx <- rep(rawprecionrecall$x[1], times=dimdefi)



dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\figure\\BBCSport plot data\\FBBCSportfoldinsinglefulldimension.eps")
par(mar=c(5,6,4,1)+.1)
plot(1:(dimdefi-1),caprecionrecall$x[1:dimdefi-1],col=1,type="l",ylim = c(0,1),
     lty=1,main = "BBCSport (single method)", xlab = "Number of dimension", ylab = "Accuracy", lwd=1.8,cex.lab=1.75, cex.axis = 1.55, cex.main=1.75)
lines(1:dimdefi,rawlsaprecionrecall$x[1:dimdefi],col=2,lty=1, lwd=1.8)
lines(1:dimdefi,nrowl1lsaprecionrecall$x[1:dimdefi],col=3,lty=1, lwd=1.8)
lines(1:dimdefi,nrowl2lsaprecionrecall$x[1:dimdefi],col=4,lty=1, lwd=1.8)
lines(1:dimdefi,tfidflsaprecionrecall$x[1:dimdefi],col=5,lty=1, lwd=1.8)
lines(1:dimdefi,rawprecionrecallx,col=6,lty=1, lwd=1.8)
#title("single method",lwd=3)

legend("bottomright",cex=1,c("CA","LSA-RAW","LSA-NROWL1","LSA-NROWL2","LSA-TFIDF",
                             "RAW"),col=c(1,2,3,4,5,6),lty=c(1,1,1,1,1,1))

dev.off()


#Not used.
# dimdefi <- 10
# setEPS()
# postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCSport data 5 fold in\\figure\\BBCSport plot data\\FBBCSportfoldinsinglepartdimension.eps")
# par(mar=c(5,6,4,1)+.1)
# plot(1:(dimdefi),caprecionrecall$x[1:dimdefi],col=1,type="b",ylim = c(0,1),
#      lty=1,main = "BBCSport (single method)", xlab = "Number of dimension", ylab = "Accuracy", lwd=1.8,cex.lab=1.75, cex.axis = 1.55, cex.main=1.75)
# lines(1:dimdefi,rawlsaprecionrecall$x[1:dimdefi],col=2,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,nrowl1lsaprecionrecall$x[1:dimdefi],col=3,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,nrowl2lsaprecionrecall$x[1:dimdefi],col=4,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,tfidflsaprecionrecall$x[1:dimdefi],col=5,type="b",lty=1, lwd=1.8)
# lines(1:dimdefi,rawprecionrecallx[1:dimdefi],col=6,type="b",lty=1, lwd=1.8)
# 
# 
# #title("single method",lwd=3)
# 
# legend("bottomright",cex=1,c("CA","LSA-RAW","LSA-NROWL1","LSA-NROWL2","LSA-TFIDF",
#                              "RAW"),col=c(1,2,3,4,5,6),lty=c(1,1,1,1,1,1),pch=c(1,1,1,1,1,1))
# 
# dev.off()
# 
