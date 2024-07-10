#For section 2 of the first paper: this code is about LSA-RAW for small dataset: 6*6 toy dataset


rm(list=ls())
library(Matrix)

### original data
X <- matrix(c(2,2,1,2,0,0,2,3,1,2,0,0,1,3,1,2,0,0,2,3,1,3,1,2,0,0,0,1,1,1,0,0,0,1,1,2),nrow = 6, ncol = 6)
rownames(X) <- c("1","2","3","4","5","6")
#rownames(X) <- c("doc1","doc2","doc3","doc4","doc5","doc6")
colnames(X) <- c("lion","tiger","cheetah","jaguar","porsche","ferrari")
X.count <- X
X.count

end <- rankMatrix(X.count)
end

#raw frequencies matrix D
X.svd <- svd(X.count)

#Equation (3)
round(X.svd$u,3)
round(X.svd$d,3)
round(X.svd$v,3)

#Table 5: The singular values, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F.
round(X.svd$d,3)
round(X.svd$d^2,3)
round(X.svd$d[1:end]^2/sum(X.svd$d[1:end]^2),3)
Variance.qqq <- X.svd$d[1:end]^2/sum(X.svd$d[1:end]^2)
Variance.qqq.firstdimensions <- Variance.qqq[1]+Variance.qqq[2]
Variance.qqq.firstdimensions



UD = X.svd$u%*%diag(X.svd$d)
VD <- X.svd$v%*%diag(X.svd$d)

 
#Figure 1: A two-dimensional plot of documents and terms for F.
dev.off()
x.min <- min(min(UD[,1]),min(X.svd$v[,1]))
y.min <- min(min(UD[,2]),min(X.svd$v[,2]))
x.max <- max(max(UD[,1]),max(X.svd$v[,1]))
y.max <- max(max(UD[,2]),max(X.svd$v[,2]))

xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1

setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\small data\\figure\\Fsmalldatasetlsaraw.eps")

col.set <- c("black","blue")
par(mar=c(5,6,4,1)+.1)
plot(UD[,1], UD[,2],type="p", cex = 1.8,pch=20,col = col.set[1],asp = 1, xlim=c(-6,0),ylim=c(-3,1.5), main="LSA-RAW", cex.main=1.75, xlab = "Dimension 1: 70.985 (85.5%)", ylab = "Dimension 2: 10.635 (12.8%)",col.lab=col.set[1],axes=F,cex.lab=1.75)
text(UD[,1]+0.2, UD[,2]-0.15, col = col.set[1], label = rownames(X.count),cex = 1.75)
lines(c(-1.2*max.total, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-1.2*max.total/2, 1.2*max.total/2), lwd=1.75, length=0.15)


k1 =ceiling(-1.2*max.total)
# k2 = floor(max.total)
k2 = floor(0)

k = 1 
for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.05,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.02,i, cex = 1.55)
  
}
k1 =ceiling(-1.2*max.total/2)
k2 = floor(1.2*max.total/2)
k = 1
for (i in seq(k1,k2,k)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.05,length.out = n),rep(i,n),type = "l")
  text(-0.02*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

#text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.55)
points(VD[,1], VD[,2], type = "p", cex = 1.8, pch = 1)
text(VD[1,1]+0.35, VD[1,2], label = colnames(X.count)[1], cex = 1.75)

text(VD[2,1]-0.2, VD[2,2]+0.2, label = colnames(X.count)[2], cex = 1.75)

text(VD[3,1]+0.35, VD[3,2]+0.2, label = colnames(X.count)[3], cex = 1.75)


text(VD[4,1], VD[4,2]+0.2, label = colnames(X.count)[4], cex = 1.75)

text(VD[5,1], VD[5,2]+0.2, label = colnames(X.count)[5], cex = 1.75)

text(VD[6,1], VD[6,2]+0.2, label = colnames(X.count)[6], cex = 1.75)


dev.off()

round(dist(UD[, 1:2], method = "euclidean"),3)
round(dist(UD, method = "euclidean"),3)
round(dist(X, method = "euclidean"),3)

