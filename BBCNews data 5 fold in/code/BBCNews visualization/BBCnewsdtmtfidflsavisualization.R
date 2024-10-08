#For Figure 3 of section 5.2 of the first paper: this code is about LSA-TFIDF.


rm(list=ls())
library(Matrix)

load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\figure\\BBCNews visualization\\rawdtmBBCnewsvisu.Rdata")
#TF matrix
TF <- as.matrix(dtm) 


#IDF matrix
pre.IDF <- dtm
pre.IDF[pre.IDF > 0.5] <- 1
#pre.IDF
IDF <- log(nrow(pre.IDF)/colSums(pre.IDF),base = 2)

#plus 1
IDF <- IDF +1

IDF <- diag(IDF)


#TF-IDF matrix

TF.IDF <- TF %*% IDF

td.mat_mfi_choose.total.svd <- svd(TF.IDF)

k <- 2
td.mat_mfi_choose.total.svd.ud <- as.matrix(td.mat_mfi_choose.total.svd$u[,1:k])%*%as.matrix(diag(td.mat_mfi_choose.total.svd$d[1:k]))

dimdefi <- rankMatrix(TF.IDF)
Variance <- 100*td.mat_mfi_choose.total.svd$d[1:dimdefi]^2/sum(td.mat_mfi_choose.total.svd$d[1:dimdefi]^2)
round((td.mat_mfi_choose.total.svd$d^2)[1:k],3)
round(Variance[1:k],1)
Variance.firstk.dimensions <- sum(Variance[1:k])
round(Variance.firstk.dimensions,3)



x.min.udvd <- min(td.mat_mfi_choose.total.svd.ud[,1])
y.min.udvd <- min(td.mat_mfi_choose.total.svd.ud[,2])
x.max.udvd <- max(td.mat_mfi_choose.total.svd.ud[,1])
y.max.udvd <- max(td.mat_mfi_choose.total.svd.ud[,2])
xlim<-c(-max(abs(x.min.udvd),abs(x.max.udvd)),max(abs(x.min.udvd),abs(x.max.udvd)))
xlim
ylim<-c(-max(abs(y.min.udvd),abs(y.max.udvd)),max(abs(y.min.udvd),abs(y.max.udvd)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1

dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\BBCNews data 5 fold in\\figure\\BBCNews visualization\\FBBCNewsLSATFIDF.eps")
par(mar=c(5,6,4,1)+.1)
plot(td.mat_mfi_choose.total.svd.ud[1:sum(index==1),1], td.mat_mfi_choose.total.svd.ud[1:sum(index==1),2], asp = 1, col = "black", cex = 1.8, type = "p", pch = 1, #xlim = c(x.min.udvd,50), ylim = c(-50,50),#xlim = c(-15,0), ylim = c(-20,20), #
     xlim = c(x.min.udvd,x.max.udvd), ylim = c(y.min.udvd,y.max.udvd), 
     main = "LSA-TFIDF",
     xlab = "Dimension 1: 1,096,902.000 (3.2%)", ylab = "Dimension 2: 586,429.800 (1.7%)",cex.lab=1.75, cex.axis = 1.55, cex.main=1.75)
points(td.mat_mfi_choose.total.svd.ud[(sum(index==1)+1):(sum(index==1)+sum(index==2)),1], td.mat_mfi_choose.total.svd.ud[(sum(index==1)+1):(sum(index==1)+sum(index==2)),2], col = "blue", type = "p", pch = 3, cex = 1.8)
points(td.mat_mfi_choose.total.svd.ud[(sum(index==1)+sum(index==2)+1):(sum(index==1)+sum(index==2)+sum(index==3)),1], td.mat_mfi_choose.total.svd.ud[(sum(index==1)+sum(index==2)+1):(sum(index==1)+sum(index==2)+sum(index==3)),2], col = "orange", type = "p", pch = 23, cex = 1.8)
points(td.mat_mfi_choose.total.svd.ud[(sum(index==1)+sum(index==2)+sum(index==3)+1):(sum(index==1)+sum(index==2)+sum(index==3)+sum(index==4)),1], td.mat_mfi_choose.total.svd.ud[(sum(index==1)+sum(index==2)+sum(index==3)+1):(sum(index==1)+sum(index==2)+sum(index==3)+sum(index==4)),2], col = "green", type = "p", pch = 0, cex = 1.8)
points(td.mat_mfi_choose.total.svd.ud[(sum(index==1)+sum(index==2)+sum(index==3)+sum(index==4)+1):(sum(index==1)+sum(index==2)+sum(index==3)+sum(index==4)+sum(index==5)),1], td.mat_mfi_choose.total.svd.ud[(sum(index==1)+sum(index==2)+sum(index==3)+sum(index==4)+1):(sum(index==1)+sum(index==2)+sum(index==3)+sum(index==4)+sum(index==5)),2], col = "Violet", type = "p", pch = 8, cex = 1.8)

legend("topright",cex=1,c('business', 'entertainment', 'politics', 'sport', 'tech'),col=c("black","blue","orange","green", "Violet"),pch = c(1, 3, 23, 0, 8))
dev.off()

