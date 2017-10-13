plot.pcovar <-
function(x,...,reversals=c(0,0,0,0,0,0)) {
    o.pcovar<- x
    rev<- (-2*reversals)+1
    y<- o.pcovar$y
    par(mfrow=c(2,3),pty="s",mar=c(0.0,2.3,2.3,0.3),omi=c(1,0,0,0),mgp=c(1.30,0.5,0),lwd=0.5)
    colors<- c("darkred","darkolivegreen4","blue","gold","darkorange")
    #    colors<- gray(seq(0.2,0.8,0.15))
    coo<- colors[o.pcovar$grel]
    co<- "black"
    
    
    # euclid
    plot(c(o.pcovar$euclidpca[,1],o.pcovar$euclidpco[,1]),c(o.pcovar$euclidpca[,2]*rev[1],o.pcovar$euclidpco[,2]*rev[1]),xlab="PCO axis 1",ylab="PCO axis 2",asp=1,type="n",cex.axis=0.8,cex.lab=0.8,tcl=-0.3)
    points(o.pcovar$euclidpca[,1],o.pcovar$euclidpca[,2]*rev[1],pch=16,cex=1.0,col=coo)
    points(o.pcovar$euclidpco[,1],o.pcovar$euclidpco[,2]*rev[1],pch=16,cex=0.2,col=co)
    abline(h=0,v=0,lwd=0.6,col="gray")
    title(substitute(paste("PCOA, Euclidean, ",italic("x'=x")^y,sep=""),list(y=y)),cex.main=1.0)
    
    # manhattan
    plot(c(o.pcovar$manhpca[,1],o.pcovar$manhpco[,1]),c(o.pcovar$manhpca[,2]*rev[2],o.pcovar$manhpco[,2]*rev[2]),xlab="PCO axis 1",ylab="PCO axis 2",asp=1,type="n",cex.axis=0.8,cex.lab=0.8,tcl=-0.3)
    points(o.pcovar$manhpca[,1],o.pcovar$manhpca[,2]*rev[2],pch=16,cex=1.0,col=coo)
    points(o.pcovar$manhpco[,1],o.pcovar$manhpco[,2]*rev[2],pch=16,cex=0.2,col=co)
    abline(h=0,v=0,lwd=0.6,col="gray")
    title(substitute(paste("PCOA, Manhattan, ",italic("x'=x")^y,sep=""),list(y=y)),cex.main=1.0)
    
    # chord
    plot(c(o.pcovar$cordpca[,1],o.pcovar$cordpco[,1]),c(o.pcovar$cordpca[,2]*rev[3],o.pcovar$cordpco[,2]*rev[3]),xlab="PCO axis 1",ylab="PCO axis 2",asp=1,type="n",cex.axis=0.8,cex.lab=0.8,tcl=-0.3)
    points(o.pcovar$cordpca[,1],o.pcovar$cordpca[,2]*rev[3],pch=16,cex=1.0,col=coo)
    points(o.pcovar$cordpco[,1],o.pcovar$cordpco[,2]*rev[3],pch=16,cex=0.2,col=co)
    abline(h=0,v=0,lwd=0.6,col="gray")
    title(substitute(paste("PCOA, Chord distance, ",italic("x'=x")^y,sep=""),list(y=y)),cex.main=1.0)
    
    # canberra
    plot(c(o.pcovar$canpca[,1],o.pcovar$canpco[,1]),c(o.pcovar$canpca[,2]*rev[4],o.pcovar$canpco[,2]*rev[4]),xlab="PCO axis 1",ylab="PCO axis 2",asp=1,type="n",cex.axis=0.8,cex.lab=0.8,tcl=-0.3)
    points(o.pcovar$canpca[,1],o.pcovar$canpca[,2]*rev[4],pch=16,cex=1.0,col=coo)
    points(o.pcovar$canpco[,1],o.pcovar$canpco[,2]*rev[4],pch=16,cex=0.2,col=co)
    abline(h=0,v=0,lwd=0.6,col="gray")
    title(substitute(paste("PCOA, Canberra, ",italic("x'=x")^y,sep=""),list(y=y)),cex.main=1.0)
    
    # bray
    plot(c(o.pcovar$bpca[,1],o.pcovar$bpco[,1]),c(o.pcovar$bpca[,2]*rev[5],o.pcovar$bpco[,2]*rev[5]),xlab="PCO axis 1",ylab="PCO axis 2",asp=1,type="n",cex.axis=0.8,cex.lab=0.8,tcl=-0.3)
    points(o.pcovar$bpca[,1],o.pcovar$bpca[,2]*rev[5],pch=16,cex=1.0,col=coo)
    points(o.pcovar$bpco[,1],o.pcovar$bpco[,2]*rev[5],pch=16,cex=0.2,col=co)
    abline(h=0,v=0,lwd=0.6,col="gray")
    title(substitute(paste("PCOA, Bray-Curtis, ",italic("x'=x")^y,sep=""),list(y=y)),cex.main=1.0)
    
    # correlation as distance
    plot(c(o.pcovar$corpca[,1],o.pcovar$corpco[,1]),c(o.pcovar$corpca[,2]*rev[6],o.pcovar$corpco[,2]*rev[6]),xlab="PCO axis 1",ylab="PCO axis 2",asp=1,type="n",cex.axis=0.8,cex.lab=0.8,tcl=-0.3)
    points(o.pcovar$corpca[,1],o.pcovar$corpca[,2]*rev[6],pch=16,cex=1.0,col=coo)
    points(o.pcovar$corpco[,1],o.pcovar$corpco[,2]*rev[6],pch=16,cex=0.2,col=co)
    abline(h=0,v=0,lwd=0.6,col="gray")
    title(substitute(paste("PCOA, (1-Correlation)/2, ",italic("x'=x")^y,sep=""),list(y=y)),cex.main=1.0)
    
}
