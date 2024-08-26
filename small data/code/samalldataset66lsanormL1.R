#For section 2 of the first paper: this code is about LSA-NROWL1 for small dataset: 6*6 toy dataset


rm(list=ls())
library(Matrix)

### original data
X <- matrix(c(2,2,1,2,0,0,2,3,1,2,0,0,1,3,1,2,0,0,2,3,1,3,1,2,0,0,0,1,1,1,0,0,0,1,1,2),nrow = 6, ncol = 6)
rownames(X) <- c("1","2","3","4","5","6")
#rownames(X) <- c("doc1","doc2","doc3","doc4","doc5","doc6")
colnames(X) <- c("lion","tiger","cheetah","jaguar","porsche","ferrari")
X.count <- X
X.count

#Table 3: A document-term matrix F^N
X.count.N <- X.count/matrix(rep(apply(X.count,1,sum),each = nrow(X.count)), ncol = ncol(X.count), by = TRUE)
round(X.count.N,3)
apply(X.count.N,1,sum)
round(apply(X.count.N,2,sum),3)

X.svd.N <- svd(X.count.N)
end <- rankMatrix(X.count.N)
end
#Equation (5)
round(X.svd.N$u,3)
round(X.svd.N$d,3)
round(X.svd.N$v,3)

#Table 2: The singular value, the squares of singular values, and the proportion of explained total sum of squared singular values (PSSSV) for each dimension of LSA of F^N.
round(X.svd.N$d,3)
round(X.svd.N$d^2,3);round(X.svd.N$d[1:end]^2/sum(X.svd.N$d[1:end]^2),3)
qqpercentage <- X.svd.N$d[1:end]^2/sum(X.svd.N$d[1:end]^2)
round(qqpercentage[1]+qqpercentage[2],3)



UD.N = X.svd.N$u%*%diag(X.svd.N$d)
VD.N <- X.svd.N$v%*%diag(X.svd.N$d)

#Figure 1: A two-dimensional plot of documents and terms for row-normalized data F^N.
dev.off()
x.min <- min(min(UD.N[,1]),min(VD.N[,1]))
y.min <- min(min(UD.N[,2]),min(VD.N[,2]))
x.max <- max(max(UD.N[,1]),max(VD.N[,1]))
y.max <- max(max(UD.N[,2]),max(VD.N[,2]))
xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim
max.total <- max(xlim[2],ylim[2])*1.1

#postscript("C:\\66biplotFNnewnodL1.eps")
col.set <- c("black","blue")
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\230402 proof read figure label\\Fsmalldatasetlsanrowl1.eps")
par(mar=c(5,6,4,1)+.1)

plot(UD.N[,1], UD.N[,2],type="p", cex = 1.8,pch=20,col = col.set[1],asp=1, #xlim=c(-max.total*2,0),ylim=c(-max.total,max.total),
     xlim=c(-0.8,0),ylim=c(-0.5,0.5), main="LSA-NROWL1", cex.main=1.75, xlab = "Dimension 1: 1.146 (69.2%)", ylab = "Dimension 2: 0.479 (28.9%)",col.lab=col.set[1],axes=F, cex.lab=1.75)
text(UD.N[1,1]-0.03, UD.N[1,2]-0.04, col = col.set[1], label = rownames(X.count)[1], cex=1.75)
text(UD.N[2,1]-0.03, UD.N[2,2], col = col.set[1], label = rownames(X.count)[2], cex=1.75)
text(UD.N[3,1]-0.03, UD.N[3,2]+0.035, col = col.set[1], label = rownames(X.count)[3], cex=1.75)
text(UD.N[4,1]-0.03, UD.N[4,2]+0.035, col = col.set[1], label = rownames(X.count)[4], cex=1.75)
text(UD.N[5,1]-0.03, UD.N[5,2]-0.04, col = col.set[1], label = rownames(X.count)[5], cex=1.75)
text(UD.N[6,1]-0.03, UD.N[6,2]+0, col = col.set[1], label = rownames(X.count)[6], cex=1.75)



lines(c(-0.85, 0), c(0, 0), lwd=1.75, length=0.15)
lines(c(0, 0), c(-max.total, max.total), lwd=1.75, length=0.15)

k1 = -0.8
#k1 =ceiling(-max.total*2+0.3)
# k2 = floor(max.total)
k2 = floor(0)

# k = 0.4 #k?????????
k = 0.2
for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.05,length.out = n),type = "l")# ??????X??????????????????
  
  text(i,-(xlim[2] - xlim[1] )*0.03,i, cex = 1.55)
  
}
k1 =-0.5
k2 = 0.5
k = 0.2
for (i in seq(k1,k2,k)) { # ??????????????????,???1?????????,k?????????
  n=5
  if(i == 0 ) next()
  lines(seq(0,k*0.05,length.out = n),rep(i,n),type = "l")
  text(-0.035*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}

#text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.55)
points(VD.N[,1], VD.N[,2], type = "p", cex = 1.8, pch = 1)
text(VD.N[1,1]+0.05, VD.N[1,2]+0.025, label = colnames(X.count)[1], cex=1.75)
text(VD.N[2,1]+0.007, VD.N[2,2]+0.043, label = colnames(X.count)[2], cex=1.75)
text(VD.N[3,1]+0.009, VD.N[3,2]-0.02, label = colnames(X.count)[3], cex=1.75)
text(VD.N[4,1]+0.062, VD.N[4,2]-0.03, label = colnames(X.count)[4], cex=1.75)
text(VD.N[5:6,1]+0.07, VD.N[5:6,2]+0.03, label = colnames(X.count)[5:6], cex=1.75)
dev.off()


