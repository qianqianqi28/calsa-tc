#For Figure 9 of section 6.2 of the first paper: this code is about LSA-NROWL2.



rm(list=ls())
library(caret)
library(e1071)
library(stringr)
library(tm)
library("readxl")
library("superml")
library("udpipe")
library("data.table")
library(Matrix)

load("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\figure\\Wilhelmus visualization\\sixauthorsLSARAWLSANROWLSATFIDFCA\\rawmatrixsixauthors.Rdata")

td.mat_mfi_choose.total.nrow <- training_dtm/matrix(rep(sqrt(apply(training_dtm^2,1,sum)),each = ncol(training_dtm)), ncol = ncol(training_dtm), by = TRUE)

testing_dtm_nrow <- testing_dtm/matrix(rep(sqrt(apply(testing_dtm^2,1,sum)),each = ncol(testing_dtm)), ncol = ncol(testing_dtm), by = TRUE)
dim(testing_dtm_nrow)


td.mat_mfi_choose.total.svd <- svd(td.mat_mfi_choose.total.nrow)



dimdefi <- rankMatrix(td.mat_mfi_choose.total.nrow)

k <- 2

Variance <- 100*td.mat_mfi_choose.total.svd$d[1:dimdefi]^2/sum(td.mat_mfi_choose.total.svd$d[1:dimdefi]^2)
round((td.mat_mfi_choose.total.svd$d^2)[1:k],3)
round(Variance[1:k],1)
Variance.firstk.dimensions <- sum(Variance[1:k])
round(Variance.firstk.dimensions,1)

td.mat_mfi_choose.total.svd.ud <- as.matrix(td.mat_mfi_choose.total.svd$u[,1:k])%*%as.matrix(diag(td.mat_mfi_choose.total.svd$d[1:k]))

test_new_matrix.svd.ud <- t(as.matrix((as.matrix(testing_dtm_nrow) %*% as.matrix(td.mat_mfi_choose.total.svd$v))[,1:k]))



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
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\Wilhelmus data fold in\\figure\\Wilhelmus visualization\\sixauthorsLSARAWLSANROWLSATFIDFCA\\FfoldinsixauthorspredictwilhelmusLSANROWL2.eps")
par(mar=c(5,6,4,1)+.1)
plot(td.mat_mfi_choose.total.svd.ud[1:length(author_datheen.vector),1], td.mat_mfi_choose.total.svd.ud[1:length(author_datheen.vector),2], asp = 1, col = "black", cex = 1.8, type = "p", pch = 1, xlim = c(x.min.udvd,x.max.udvd), ylim = c(y.min.udvd,y.max.udvd), 
     main = "LSA-NROWL2",
     xlab = "Dimension 1: 119.129 (64.0%)", ylab = "Dimension 2: 18.610 (10.0%)",cex.lab=1.75, cex.axis = 1.55, cex.main=1.75)
points(td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)),1], td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)),2], col = "blue", type = "p", pch = 3, cex = 1.8)
points(td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)),1], td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)),2], col = "orange", type = "p", pch = 23, cex = 1.8)
points(td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)),1], td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)),2], col = "green", type = "p", pch = 0, cex = 1.8)
points(td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+length(author_fruytiers.vector)),1], td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+length(author_fruytiers.vector)),2], col = "Violet", type = "p", pch = 8, cex = 1.8)
points(td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+length(author_fruytiers.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+length(author_fruytiers.vector)+length(author_coornhert.vector)),1], td.mat_mfi_choose.total.svd.ud[(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+length(author_fruytiers.vector)+1):(length(author_datheen.vector)+length(author_marnix.vector)+length(author_heere.vector)+length(author_haecht.vector)+length(author_fruytiers.vector)+length(author_coornhert.vector)),2], col = "cyan", type = "p", pch = 2, cex = 1.8)

points(test_new_matrix.svd.ud[1,1], test_new_matrix.svd.ud[1,2], col = "red", type = "p", pch = 20, cex = 1.8)
text(test_new_matrix.svd.ud[1,1], test_new_matrix.svd.ud[1,2],label = "W",col = "red", cex = 1.75)

legend("right",cex=1,c("Datheen","Marnix","Heere","Haecht","Fruytiers","Coornhert"),col=c("black","blue","orange","green", "Violet", "cyan"),pch = c(1, 3, 23, 0, 8, 2))


dev.off()
