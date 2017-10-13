plot.overly<- function(x,...,colors=NULL,l.widths=NULL) {
    o.overly<- x
    tree<- o.overly$tree
    out<- pco(o.overly$d.mat,k=2)
    plot(out$points[,1],out$points[,2],xlab="PCOA axis 1",ylab="PCOA axis 2",asp=1,cex.axis=0.7,cex.lab=0.7,mgp=c(1.8,0.4,0))
    abline(h=0,v=0,lwd=1.0,col="gray")
    lines(tree,ord=out,display="sites",col="lightblue",lwd=1.5)    # minimum spanning tree in ordination
    pos.corr<-c(3,1,1,1,3,1,2,3,3,3,3,3,3,3,4,3,2,2,1,4,3,3,3,3,3,3,3,4,3,3,3,
    3,3,3,4,3,3,3,2,3,3,2,3,3,3,4,3,3,3,2,2,3,3,4,3,2,4,3,3)
    text(out$points[,1],out$points[,2],o.overly$plot.labels,pos=pos.corr,cex=0.6)
    #
    # plot of alignment of time series
    # --------------------------------
    veg<- o.overly$vegraw
    nspec<- ncol(veg)
    nt<- ncol(o.overly$d.mat)
    range<- o.overly$n.tsteps
    plot(c(0,range*1.05),c(0,nt),type="n",xlab="Time step no.",ylab="Time series",cex.lab=0.7)
    for (i in 1:nt) {
        lines(c(o.overly$linex1[i],o.overly$linex2[i]),c(i,i),lwd=2.5,col="gray")
        text(o.overly$linex2[i],i,o.overly$ltext[i],pos=4,cex=0.6)
    }
    abline(v=0,lwd=1.0,col="gray")
    #
    # plot of final time series
    # -------------------------
    # The next 2 lines set defaults if linewidths are null
    #  nlines<- dim(o.overly$vegraw)[2]
    defwidth<- is.null(l.widths)
    if(defwidth == TRUE) ll.widths<- seq(0.5,4.0,0.5)
    if(defwidth != TRUE) ll.widths<- l.widths
    # The same now with colors
    defcol<- is.null(colors)
    if(defcol == TRUE) c.colors<- c("darkred","red1","darkorange","gold","lightgreen","darkolivegreen4")
    if(defcol != TRUE) c.colors<- colors
    #
    sint<- o.overly$sint
    M<- o.overly$tser.data
    vegtypes<- o.overly$vegtypes
    timescal<- seq(0,(range-1)*sint,sint)
    par(mfrow=c(1,1),omi=c(2,0,0,0))
    plot(c(0,range*sint),c(0,max(M)),xlab="Time units",ylab="Cover scale",type="n")
    for(i in 1:nspec) lines(timescal,M[,i],col=c.colors[i],lwd=ll.widths[i],lty=1)
    legend("topleft",vegtypes,lty=1,lwd=ll.widths,col=c.colors,ncol=2,bty="n",cex=0.8,text.font=3)
}

