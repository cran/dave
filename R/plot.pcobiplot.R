plot.pcobiplot <-
function(x,...,axes=c(1,2),sel.sp=NULL,shortnames=TRUE) {
   o.pcobiplot<- x
# taking a random selection of species if not specified, otherwise the selection indicated
    ax<- is.null(axes)
    if(ax == TRUE) axes<- c(1,2)
    ts<- is.null(sel.sp)
    if(ts == TRUE) sel.sp<- sample(seq(1,o.pcobiplot$nspec),6) ; snames<- o.pcobiplot$allspnames[sel.sp]
    if(ts == FALSE) snames<- o.pcobiplot$allspnames[sel.sp]
    if(shortnames == TRUE) snames <- make.cepnames(snames)
# plot A
    par(mfrow=c(1,1),omi=c(0,0,0,0),mar=c(4,4,2,2),mgp=c(1.5,0.5,0),pty="s",cex.axis=0.8,lwd=0.6,cex.lab=0.8)
    plot(o.pcobiplot$rpoints[,axes[1]],o.pcobiplot$rpoints[,axes[2]],xlab=paste("PCOA axis",axes[1]),ylab=paste("PCOA axis ",axes[2]),asp=1,cex.axis=0.8,cex.lab=0.8,tcl=-0.2,pch=18)
    abline(h=0,v=0,lwd=1.0,col="gray")
    legend("topleft","A",cex=1.5,bty="n",inset=c(-0.05,-0.02))
# plot B
    plot(o.pcobiplot$spoints[axes[1],sel.sp],o.pcobiplot$spoints[axes[2],sel.sp],type="n",xlab=paste("PCOA axis",axes[1]),ylab=paste("PCOA axis ",axes[2]),cex.lab=0.8,cex.axis=0.8)
    text(o.pcobiplot$spoints[axes[1],sel.sp],o.pcobiplot$spoints[axes[2],sel.sp],snames,cex=1/(log10(length(sveg[1,])))*1.0,pos=1)
    for(i in 1:9) arrows(0,0,o.pcobiplot$spoints[axes[1],sel.sp[i]],o.pcobiplot$spoints[axes[2],sel.sp[i]],lwd=0.5,length=0.08,angle=20)
    abline(h=0,v=0,lwd=1.0,col="gray")
    legend("topleft","B",cex=1.5,bty="n",inset=c(-0.05,-0.02))
  }
