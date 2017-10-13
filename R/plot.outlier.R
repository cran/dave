plot.outlier<- function(x,...) {
    o.outlier<- x
    neighb<- o.outlier$neigh.dist
    dmm<- o.outlier$olddim[1]
    par(omi=c(1,0,1.5,0))
    o.h<- hist(neighb,xlab="Nearest neighbour distance",main="",col=gray(0.8),labels=T,ylim=c(0,dmm*0.6),mgp=c(2,0.5,0))
    # fitting an normal distribution curve
    colwidths<- o.h$mids[2]-o.h$mids[1]
    drange<- range(neighb)
    ddrange<- range(neighb)[2]-range(neighb)[1]
    xline<- seq(drange[1],drange[2],ddrange/100)
    yline<- dnorm(xline,mean=mean(neighb),sd=sd(neighb))*length(neighb)*colwidths
    lines(xline,yline,col="red3",lwd=1.2)
    
    # ordination
    x.c<- o.outlier$pco.points[,1]
    y.c<- o.outlier$pco.points[,2]
    thresh<- o.outlier$threshold
    par(omi=c(0,0,0,0))
    plot(x.c,y.c,asp=1,type="n",cex.axis=0.7,cex.lab=1.0,tcl=-0.3,mgp=c(2,0.5,0))
    points(x.c[neighb <= thresh],y.c[neighb <= thresh],pch=16,cex=0.6,col=gray(0.85))
    points(x.c[neighb > thresh],y.c[neighb > thresh],pch=16,cex=0.6,col="red4")
    abline(h=0,v=0,lwd=1.0,col="black")
    legend("bottomleft",c("outliers","other data points"),pch=c(16,16),col=c("red4",gray(0.85)),bty="n",cex=0.8)
    #     legend("topleft","A",cex=1.5,bty="n",inset=c(-0.05,-0.02))
}
