#For section 2 of the first paper: this code is about LSA-TFIDF for small dataset: 6*6 toy dataset


rm(list=ls())
library(Matrix)



X <- matrix(c(2,2,1,2,0,0,2,3,1,2,0,0,1,3,1,2,0,0,2,3,1,3,1,2,0,0,0,1,1,1,0,0,0,1,1,2),nrow = 6, ncol = 6)
rownames(X) <- c("1","2","3","4","5","6")
colnames(X) <- c("lion","tiger","cheetah","jaguar","porsche","ferrari")
X.count <- X
TF <- X.count
TF.count <- as.matrix(TF)

row.name <- rownames(X)
col.name <- colnames(X)

TF <- as.matrix(TF) 
round(TF,2)
rownames(TF) <- row.name
rownames(TF)
colnames(TF) <- col.name
colnames(TF)

#IDF matrix
pre.IDF <- TF.count
pre.IDF[pre.IDF > 0.5] <- 1
pre.IDF
nrow(pre.IDF)
colSums(pre.IDF)
IDF <- log(nrow(pre.IDF)/colSums(pre.IDF),2)

#plus 1
IDF <- IDF +1

IDF <- diag(IDF)
colnames(IDF) <- col.name
round(IDF,2)

#TF-IDF matrix

#Table 8: A document-term matrix F^TF-IDF
TF.IDF <- TF %*% IDF
rownames(TF.IDF) <- row.name
colnames(TF.IDF) <- col.name
round(TF.IDF,3)
apply(TF.IDF,2,sum)

TF.IDF.svd <- svd(TF.IDF)
end <- rankMatrix(TF.IDF)
end
#Equation (6)
round(TF.IDF.svd$u,3)
round(TF.IDF.svd$d,3)
round(TF.IDF.svd$v,3)

#Table 9: The singular value, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F^TF-IDF.
round(TF.IDF.svd$d,3)
round(TF.IDF.svd$d^2,3);round((TF.IDF.svd$d[1:end]^2/sum(TF.IDF.svd$d[1:end]^2)),3)
QQUSE<-TF.IDF.svd$d[1:end]^2/sum(TF.IDF.svd$d[1:end]^2)
round(QQUSE[1]+QQUSE[2],3)


#Figure 3: A two-dimensional plot of documents and terms for matrix F^TF-IDF.

UD <- TF.IDF.svd$u %*% diag(TF.IDF.svd$d)
VD <- TF.IDF.svd$v %*% diag(TF.IDF.svd$d)

dev.off()
x.min <- min(min(UD[,1]),min(TF.IDF.svd$v[,1]))
y.min <- min(min(UD[,2]),min(TF.IDF.svd$v[,2]))
x.max <- max(max(UD[,1]),max(TF.IDF.svd$v[,1]))
y.max <- max(max(UD[,2]),max(TF.IDF.svd$v[,2]))

xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1

setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\small data\\figure\\Fsmalldatasetlsatfidf.eps")

col.set <- c("black","blue")
par(mar=c(5,6,4,1)+.1)

plot(UD[,1], UD[,2],type="p", cex = 1.8,pch=20,col = col.set[1],asp = 1, #xlim=c(-max.total,0),ylim=c(-max.total/2,max.total/2),
     xlim=c(-8,0),ylim=c(-5, 2), main="LSA-TFIDF", cex.main=1.75, xlab = "Dimension 1: 141.088 (78.6%)", ylab = "Dimension 2: 34.782 (19.4%)",col.lab=col.set[1],axes=F,cex.lab=1.75)
text(UD[1:4,1], UD[1:4,2]-0.3, col = col.set[1], label = rownames(X.count)[1:4],cex = 1.75)
text(UD[5,1], UD[5,2]-0.3, col = col.set[1], label = rownames(X.count)[5],cex = 1.75)
text(UD[6,1], UD[6,2]-0.3, col = col.set[1], label = rownames(X.count)[6],cex = 1.75)
lines(c(-1.2*max.total, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-1.2*max.total, 1.2*max.total), lwd=1.75, length=0.15)


k1 =ceiling(-1.2*max.total)

k2 = floor(0)

k = 1 
for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.08,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.02,i, cex = 1.55)
  
}
k1 =ceiling(-1.2*max.total/2)
k2 = floor(1.2*max.total/2)
k = 1
for (i in seq(k1,k2,k)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.08,length.out = n),rep(i,n),type = "l")
  text(-0.02*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

#text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.55)
points(VD[,1], VD[,2], type = "p", cex = 1.8, pch = 1)
text(VD[1,1]+0.2, VD[1,2]-0.35, label = colnames(X.count)[1], cex = 1.75)
text(VD[2,1], VD[2,2]+0.35, label = colnames(X.count)[2], cex = 1.75)
text(VD[3,1]+0.35, VD[3,2]+0.35, label = colnames(X.count)[3], cex = 1.75)
text(VD[4,1], VD[4,2]+0.35, label = colnames(X.count)[4], cex = 1.75)
text(VD[5,1]-0.35, VD[5,2]+0.35, label = colnames(X.count)[5], cex = 1.75)
text(VD[6,1], VD[6,2]+0.35, label = colnames(X.count)[6], cex = 1.75)


dev.off()

