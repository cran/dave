plot.vvelocity<- function(x,tlabs,scal=1,...) {
    o.vvelocity<- x
    pveg<- o.vvelocity$pveg
    timescale<- o.vvelocity$timescale
    y<- o.vvelocity$y
    # speed graph
    # -----------
    ntim <- length(pveg[,1])                                   # number of time steps
    hcol<- heat.colors(ntim*1.5)
    #  hcol<- gray(seq(0.4,0.9,0.5/ntim))
    time<- as.double(timescale)                                # the time vector
    y<- 0.5
    dmat<- dist(pveg^y,method="euclidean",diag=TRUE,upper=TRUE)      # distance matrix of time steps
    dmat<- as.matrix(dmat)
    size<- rep(0,ntim)
    for (i in 2:ntim) size[i]<- dmat[i-1,i]                    # taking elements below the diagonal of dmat
    size[1]<- size[2]                                          # first element the same as second
    out<- pco(dmat)
    par(lwd=0.5)
    plot(out$points[,1]*scal,out$points[,2]*scal,asp=1,cex.axis=0.8,cex.lab=0.8,ylab="PCO axis 2",xlab="PCO axis 1",type="n",frame.plot=TRUE,tcl=-0.3,mgp=c(1.5,0.5,0))
    points(out$points[,1],out$points[,2],pch=21,bg=hcol,cex=size*3)
    text(out$points[tlabs,1],out$points[tlabs,2],time[tlabs],pos=c(4,3,4,3,2,2,2),offset=1.5,cex=0.8)
    abline(h=0,v=0,col="gray")
    legend("bottomleft",c("Start","End"),pch=21,pt.bg=c(hcol[1],hcol[ntim]),pt.cex=1.2,bty="n",cex=0.6)
    legend("topleft","(a)",bty="n",cex=1.0,inset=c(-0.04,-0.00))
    # legend("topleft",expression(paste("PCoA, x' = ",x^y)),bty="n",cex=0.8)
    # acceleration graph
    # ------------------
    colblue<- rgb(0,0,255,100,maxColorValue=255)
    #  colblue<- gray(0.6)
    for (i in 2:ntim) size[i]<- dmat[i-1,i]
    accel<- rep(0,ntim)
    shade<- rep(colblue,ntim)
    for (i in 2:ntim){
        accel[i]<- size[i]-size[i-1]
        if (accel[i] <= 0.0) shade[i]<- "white"
    }
    par(lwd=0.5)
    plot(out$points[,1]*scal,out$points[,2]*scal,asp=1,cex.axis=0.8,cex.lab=0.8,ylab="PCO axis 2",xlab="PCO axis 1",type="n",frame.plot=TRUE,tcl=-0.3,mgp=c(1.5,0.5,0))
    points(out$points[,1],out$points[,2],pch=21,bg=shade,cex=abs(accel)*20)
    text(out$points[tlabs,1],out$points[tlabs,2],time[tlabs],pos=c(4,3,4,3,2,2,2),offset=1.5,cex=0.8)
    abline(h=0,v=0,col="gray")
    legend("bottomleft",c("Acceleration, positive","Acceleration, negative"),pch=21,pt.bg=c(colblue,"white"),pt.cex=1.2,bty="n",cex=0.6)
    legend("topleft","(b)",bty="n",cex=1.0,inset=c(-0.04,-0.00))
}
