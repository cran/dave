plot.mxplot<- function(x,...,capacity=100) {
    #
    # And here the version using image()
    #
    o.mx<- x
    col=gray(c(seq(0,1,0.05)))                           # gray scale replacing default
    col<- heat.colors(50)
    image(-o.mx$mmatrix[,o.mx$order:1]^0.25,asp=1,xaxt="n",yaxt="n",bty="n",col=col)
    lnb<- c(1:o.mx$order)                                # x-axis labels
    hst<- 1/o.mx$order
    hst<-hst*1.01
    x<- (lnb*hst)-0.45*hst
    y<- c(rep(-2*hst,o.mx$order))
    text(x,y,c(1:o.mx$order),xpd=NA,cex=0.5,srt=90)      # xpd expands plotting area
    x<- c(rep(-2*hst,o.mx$order))
    y<- (c(o.mx$order:1)*hst)-0.85*hst
    text(x,y,c(1:o.mx$order),xpd=NA,cex=0.5,srt=0)       # xpd expands plotting area
    x<- c(-hst/2,1+hst/2,1+hst/2,-hst/2,-hst/2)
    y<- c(-hst/2,-hst/2,1+hst/2,1+hst/2,-hst/2)
    lines(x,y,lwd=0.5)
}

