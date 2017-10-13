plot.aocc<- function(x,...) {
    A<- as.data.frame(x$cont.table)
    plot(c(x$sgrscores[,1],x$rgrscores[,1]),c(x$sgrscores[,2],x$rgrscores[,2]),type="n",xlab="AOC axis 1",ylab="AOC axis 2",asp=1,cex.axis=0.8,cex.lab=0.8,mgp=c(2.0,0.5,0))
    #
    colrgr<- c("darkblue")
    colrgs<- c("darkorange")
    #    colrgr<- gray(0.5)
    #    colrgs<- gray(0.7)
    #
    abline(h=0, v=0, col="gray")
    points(x$sgrscores[,1],x$sgrscores[,2],pch=16,col=colrgs,cex=3)
    text(x$sgrscores[,1],x$sgrscores[,2],row.names(A),cex=0.8,col="black")
    points(x$rgrscores[,1],x$rgrscores[,2],pch=17,col=colrgr,cex=3)
    text(x$rgrscores[,1],x$rgrscores[,2],colnames(A),cex=0.8,col="white")
    legend("topleft",pch=c(17,16),col=c(colrgr,colrgs),pt.cex=c(1.2,1.2),paste(c("Releve groups","Species groups")),bty="n",ncol=2,cex=0.8)
}
