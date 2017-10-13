plot.fspa<- function(x,...,axes=c(1,2)) {
    o.fspa<- x
    ngroups<- length(table(o.fspa$symbols))
    colors<- rep("black",ngroups)
    colors<- rainbow(ngroups)                                                 # for high number of groups
    colors<- c("darkred","darkolivegreen4","blue","goldenrod2","darkorange")  # colors for the book
    mysymbols<- letters[o.fspa$symbols]
    
    par(mfrow=c(2,2),omi=c(0,0,0,0),mar=c(2.0,2.0,1,1),mgp=c(1.2,0.3,0),pty="s",cex.axis=0.6,lwd=0.6,cex.lab=0.7,tcl=-0.3)
    #
    # graph A
    a1<- axes[1] ; a2<- axes[2]
    plot(o.fspa$oldpoints[,a1],o.fspa$oldpoints[,a2],asp=1,xlab=paste("PCOA axis ",a1),ylab=paste("PCOA axis ",a2),cex=0.6,col="black",type="n")
    #  legend("topleft","(a)",cex=1.2,bty="n",inset=c(-0.08,-0.02))
    legend("bottomleft",paste("d.rev=",o.fspa$d.rev),bty="n",cex=0.8)
    #  title(paste("PCOA"),cex.main=1.0,font.main=1)
    op<- par(lwd=0.5,col=gray(0.8))
    # putting coordinates of lines into one vector, xl, yl
    xl<- c()
    yl<- c()
    na<- NA
    for(k in 1:o.fspa$nline) {
        i<- o.fspa$startline[k]
        j<- o.fspa$endline[k]
        xl<- c(xl,c(o.fspa$oldpoints[i,a1],o.fspa$oldpoints[j,a1]),na)
        yl<- c(yl,c(o.fspa$oldpoints[i,a2],o.fspa$oldpoints[j,a2]),na)
    }
    lines(xl,yl)
    
    points(o.fspa$oldpoints[,a1],o.fspa$oldpoints[,a2],pch=mysymbols,cex=1.0,col=colors[o.fspa$symbols],lwd=0.6)
    #  points(o.fspa$oldpoints[,a1],o.fspa$oldpoints[,a2],pch=o.fspa$symbols,cex=0.8,col="black",lwd=0.6)
    #
    # graph B
    par(lwd=0.5,col="black")
    plot(o.fspa$newpoints[,a1],-o.fspa$newpoints[,a2],asp=1,xlab=paste("FSPA axis ",a1),ylab=paste("FSPA axis ",a2),cex=0.6,type="n")
    points(o.fspa$newpoints[,a1],-o.fspa$newpoints[,a2],pch=mysymbols,col=colors[o.fspa$symbols],cex=1.0)
    #  title(paste("FSPA"),cex.main=1.0,font.main=1)
    #  legend("topleft","(b)",cex=1.2,bty="n",inset=c(-0.08,-0.02),text.col="black")
    legend("bottomleft",paste("d.rev=",o.fspa$d.rev),bty="n",cex=0.8,text.col="black")
}