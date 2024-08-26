#For section 3 of the first paper: this code is about CA for small dataset: 6*6 toy dataset


rm(list=ls())
library(Matrix)

X <- matrix(c(2,2,1,2,0,0,2,3,1,2,0,0,1,3,1,2,0,0,2,3,1,3,1,2,0,0,0,1,1,1,0,0,0,1,1,2),nrow = 6, ncol = 6)
rownames(X) <- c("1","2","3","4","5","6")
colnames(X) <- c("lion","tiger","cheetah","jaguar","porsche","ferrari")
X.count <- X
DT <- X.count

#Table 1: A document-term matrix F
X.count

DT <- as.matrix(DT) 
apply(DT, 1, sum)
apply(DT, 2, sum)
sum(apply(DT, 2, sum))



### Correspondence matrix

#The matrix P of joint observed proportions
DT.P    <- DT/sum(DT)
round(DT.P, 3)
sum(DT.P)

### Row and column masses
DT.r    <- apply(DT.P, 1, sum)
round(DT.r,3)
DT.c    <- apply(DT.P, 2, sum)
round(DT.c,3)

### CA Step 1: the matrix S
DT.Dr   <- diag(DT.r)
DT.Dc   <- diag(DT.c)
DT.Drmh <- diag(1/sqrt(DT.r))
DT.Dcmh <- diag(1/sqrt(DT.c))
DT.P   <- as.matrix(DT.P)

#Row profiles of F
round(diag(1/DT.r) %*% DT.P,3) 
apply(diag(1/DT.r) %*% DT.P, 1, sum)

#The matrix E of expected proportions under independence
round(DT.r%o%DT.c, 3)

#The matrix of standardized residuals
DT.S   <- DT.Drmh%*%(DT.P-DT.r%o%DT.c)%*%DT.Dcmh
round(DT.S,3)
end <- rankMatrix(DT.S)
end

### CA step 2: the SVD of S
DT.svd <- svd(DT.S)
### CA Steps 3 & 4: standard row and column coordinates
DT.rsc <- DT.Drmh%*%DT.svd$u
DT.csc <- DT.Dcmh%*%DT.svd$v
### CA Steps 5 & 6:principal row and column coordinates
DT.rpc <- DT.rsc%*%diag(DT.svd$d)
DT.cpc <- DT.csc%*%diag(DT.svd$d)
### CA Step 7: principal inertias (eigenvalues) and %s

#Equation (16)
round(DT.svd$u,3)
round(DT.svd$d,3)
round(DT.svd$v,3)

#The singular values, the inertia, and the proportions of explained total inertia for each dimension of CA.
round(DT.svd$d,3)
round(DT.svd$d^2,3);round(DT.svd$d[1:end]^2/sum(DT.svd$d[1:end]^2),3)
inertia.square <- DT.svd$d[1:end]^2
inertia.two <- sum(inertia.square[1:2])/sum(inertia.square)
inertia.two


#Figure 2: Symmetric map of the data of Table 1.
x.min <- min(min(DT.rpc[,1]),min(DT.cpc[,1]))
y.min <- min(min(DT.rpc[,2]),min(DT.cpc[,2]))
x.max <- max(max(DT.rpc[,1]),max(DT.cpc[,1]))
y.max <- max(max(DT.rpc[,2]),max(DT.cpc[,2]))
xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim

max.total <- max(xlim[2],ylim[2])*1.1
max.total
dev.off()
setEPS()
postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\small data\\figure\\Fsmalldatasetcasym.eps")

col.set <- c("black","blue")
par(mar=c(5,6,4,1)+.3)




plot(DT.rpc[,1], DT.rpc[,2],type="p", cex = 1.8,pch=20,col = col.set[1],asp = 1, #xlim=c(-max.total,max.total),ylim=c(-max.total,max.total),
     xlim=c(-0.9,1.5),ylim=c(-0.3,0.3), xlab = "Dimension 1: 0.475 (93.2%)", ylab = "Dimension 2: 0.017 (3.4%)",col.lab=col.set[1],axes=F, cex.lab=1.75)
text(DT.rpc[1,1]+0.05, DT.rpc[1,2], label = rownames(X.count)[1], cex=1.75)
text(DT.rpc[2,1]+0.05, DT.rpc[2,2]-0.06, label = rownames(X.count)[2], cex=1.75)
text(DT.rpc[3,1]+0.05, DT.rpc[3,2]+0.07, label = rownames(X.count)[3], cex=1.75)
text(DT.rpc[4,1]+0.04, DT.rpc[4,2]-0.025, label = rownames(X.count)[4], cex=1.75)
text(DT.rpc[5,1]+0.03, DT.rpc[5,2]-0.08, label = rownames(X.count)[5], cex=1.75)
text(DT.rpc[6,1]-0.04, DT.rpc[6,2], label = rownames(X.count)[6], cex=1.75)
#lines(c(-max.total-0.05, max.total+0.05), c(0, 0), lwd=1.75)
#lines(c(0, 0), c(-max.total-0.05, max.total+0.05), lwd=1.75)
lines(c(-0.83-0.2, 1.63+0.05), c(0, 0), lwd=1.75)
lines(c(0, 0), c(-0.4-0.05, 0.4+0.05), lwd=1.75)
#k1 =ceiling(-max.total)-1
#k2 = ceiling(max.total)
k1 =-0.9
k2 = 1.51

k=0.3
for (i in seq(k1,k2,k)) {
  i = round(i,1)
  n=5
  if(i == 0) next()
  lines(rep(i,n),seq(0,k*0.055,length.out = n),type = "l")
  text(i,-(xlim[2] - xlim[1] )*0.025,i, cex = 1.55)
  
}

k1 =ceiling(-max.total)-1
k1
k2 = ceiling(max.total)
k = 0.2

for (i in seq(-0.4,0.4,k)) { 
  n=5
  if(abs(i) < 0.01 ) next()
  lines(seq(0,k*0.06,length.out = n),rep(i,n),type = "l")
  i
  text(-0.04*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}
#text(-(xlim[2] - xlim[1])*0.017,-(ylim[2] - ylim[1])*0.09,"O", cex = 1.55)
points(DT.cpc[,1], DT.cpc[,2], type = "p", cex = 1.8, pch = 1)
text(DT.cpc[1,1]-0.12, DT.cpc[1,2]+0.06, label = colnames(X.count)[1], cex=1.75)
text(DT.cpc[2,1]-0.12, DT.cpc[2,2]+0.06, label = colnames(X.count)[2], cex=1.75)
text(DT.cpc[3,1]-0.11, DT.cpc[3,2]-0.05, label = colnames(X.count)[3], cex=1.75)
text(DT.cpc[4,1]+0.1, DT.cpc[4,2]+0.06, label = colnames(X.count)[4], cex=1.75)
text(DT.cpc[5,1]-0.18, DT.cpc[5,2]-0.08, label = colnames(X.count)[5], cex=1.75)
text(DT.cpc[6,1]-0.05, DT.cpc[6,2]+0.14, label = colnames(X.count)[6], cex=1.75)
dev.off()



#Figure 2: Asymmetric map of the data of Table 1.
x.min <- min(min(DT.rpc[,1]),min(DT.csc[,1]))
y.min <- min(min(DT.rpc[,2]),min(DT.csc[,2]))
x.max <- max(max(DT.rpc[,1]),max(DT.csc[,1]))
y.max <- max(max(DT.rpc[,2]),max(DT.csc[,2]))
xlim<-c(-max(abs(x.min),abs(x.max)),max(abs(x.min),abs(x.max)))
xlim
ylim<-c(-max(abs(y.min),abs(y.max)),max(abs(y.min),abs(y.max)))
ylim

max.total <- max(xlim[2],ylim[2])*1.1
max.total
dev.off()

setEPS()


postscript("C:\\Users\\qi000005\\OneDrive - Universiteit Utrecht\\qi000005\\paper 1\\20220417 Review 2 archive\\small data\\figure\\Fsmalldatasetcaasym.eps")

col.set <- c("black","blue")
par(mar=c(5,6,4,1)+.1)


plot(DT.rpc[,1], DT.rpc[,2],type="p", cex = 1.8,pch=20,col = col.set[1],asp = 1, #xlim=c(-max.total-0.34,max.total+0.34),ylim=c(-max.total-0.34,max.total+0.34),
     xlim=c(-2.5,2.5),ylim=c(-2,1.5), xlab = "Dimension 1: 0.475 (93.2%)", ylab = "Dimension 2: 0.017 (3.4%)",col.lab=col.set[1],axes=F, cex.lab=1.75)

text(DT.rpc[1,1]+0.09, DT.rpc[1,2]+0.14, label = rownames(X.count)[1], cex=1.75)
text(DT.rpc[2,1], DT.rpc[2,2]-0.15, label = rownames(X.count)[2], cex=1.75)
text(DT.rpc[3,1]+0.09, DT.rpc[3,2]+0.12, label = rownames(X.count)[3], cex=1.75)
text(DT.rpc[4,1]+0.1, DT.rpc[4,2]-0.07, label = rownames(X.count)[4], cex=1.75)
text(DT.rpc[5,1]-0.05, DT.rpc[5,2]-0.12, label = rownames(X.count)[5], cex=1.75)
text(DT.rpc[6,1]-0.09, DT.rpc[6,2]+0.04, label = rownames(X.count)[6], cex=1.75)


lines(c(-max.total-0.2, max.total+0.8), c(0, 0), lwd=1.75)
lines(c(0, 0), c(-max.total-0.2, max.total+0.2), lwd=1.75)

k1 =ceiling(-max.total)-1
k1
k2 = ceiling(max.total)

k = 1


for (i in seq(k1,k2,k)) {
  n=5
  if(i == 0 ) next()
  lines(rep(i,n),seq(0,k*0.055,length.out = n),type = "l")
  
  text(i,-(xlim[2] - xlim[1] )*0.03,i, cex = 1.55)
  
}

k1 =ceiling(-max.total)-1
k1
k2 = ceiling(max.total)
k = 1 
for (i in seq(k1,k2,k)) { 
  n=5
  if(abs(i) < 0.01 ) next()
  lines(seq(0,k*0.06,length.out = n),rep(i,n),type = "l")
  text(-0.04*(xlim[2] - xlim[1]),i,i, cex = 1.55)
}
#text(-(xlim[2] - xlim[1])*0.03,-(ylim[2] - ylim[1])*0.03,"O", cex = 1.55)
points(DT.csc[,1], DT.csc[,2], type = "p", cex = 1.8, pch = 1)
text(DT.csc[1,1], DT.csc[1,2]+0.15, label = colnames(X.count)[1], cex=1.75)
text(DT.csc[2,1]-0.3, DT.csc[2,2]+0.15, label = colnames(X.count)[2], cex=1.75)
text(DT.csc[3,1], DT.csc[3,2]+0.15, label = colnames(X.count)[3], cex=1.75)
text(DT.csc[4,1]+0.3, DT.csc[4,2]+0.15, label = colnames(X.count)[4], cex=1.75)
text(DT.csc[5,1], DT.csc[5,2]+0.15, label = colnames(X.count)[5], cex=1.75)
text(DT.csc[6,1], DT.csc[6,2]+0.15, label = colnames(X.count)[6], cex=1.75)

dev.off()




