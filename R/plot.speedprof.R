plot.speedprof<- function(x,...) {
    o.speedprof<- x
    # par(mfrow=c(1,1),omi=c(2,0,0,0))
    # matrixplot was here
    dimde<- o.speedprof$nrel
    orders<- o.speedprof$orders
    maxord<- length(orders)
    mde<- o.speedprof$dmatrix
    timescale<- as.vector(o.speedprof$timescale)
    # now speedprofiles
    dd<- dimde-1
    dvec<- rep(1,dimde)
    linew<- seq(1,maxord,1)
    linew<- (linew*0.8)^1.5
    # the exponent above causes spread of line width
    
    dvec<- diag(mde[1:dimde-1,2:dimde])
    
    plot(c(min(timescale),max(timescale)),c(0,max(dvec)*1.1),type="n",xlab="Time scale",ylab="Rate of change",tcl=-0.3,mgp=c(2,0.5,0),cex.axis=0.8)
    colors<- rgb(0,0,0,seq(240,20,-(220/maxord+1)),maxColorValue=255)
    # colour version
    colors<- heat.colors(maxord*1.5)
    for (io in 1:maxord) {
        ibeg<- 1+orders[io]
        iend<- dimde-orders[io]
        dvec<- diag(mde[1:iend,ibeg:dimde])
        dvec<- dvec/orders[io]
        lines(timescale[ibeg:dimde],dvec,lwd=linew[io],col=colors[io])
        lines(c(timescale[1], timescale[ibeg]),c(dvec[1],dvec[1]),lwd=linew[io],col=colors[io],lty="dotted")
    }
    legend("top",as.character(orders),title="Order (time steps involved)",lwd=linew,col=colors[1:maxord],bty="n",ncol=5,cex=0.8)
}
