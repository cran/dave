plot.pcaser<- function(x,lines=TRUE,arrows=TRUE,...) {
    o.pcaser<- x
    usecolors<-  "TRUE"
    #
    par(mgp=c(1.10,0.3,0))
    plot(o.pcaser$scores[,1]*1.05,o.pcaser$scores[,2]*1.1,type="n",xlab="PCA axis 1",ylab="PCA axis 2",asp=1,cex.axis=0.6,cex.lab=0.8,tcl=-0.3)
    ser<- o.pcaser$plotlab
    # colorarray colarr
    colarr<- rep("black",o.pcaser$nrel)
    legcol<- rep("black",o.pcaser$nser)
    colarr[1] <- "blue"
    colarr[o.pcaser$nrel] <- "red"
    for (i in 2:o.pcaser$nrel){
        if(ser[i] != ser[i-1]) {
            colarr[i] <- "blue" ; colarr[i-1] <- "red"
        }
    }
    
    abline(h=0,v=0,lwd=1.0,col="gray")
    xfrom<- o.pcaser$scores[which(colarr=="blue"),1]
    yfrom<- o.pcaser$scores[which(colarr=="blue"),2]
    xto<- o.pcaser$scores[which(colarr=="red"),1]
    yto<- o.pcaser$scores[which(colarr=="red"),2]
    
    colarr<- rep("black",o.pcaser$nrel)
    if(usecolors == "TRUE") {
        legcol<- rain<- rainbow(o.pcaser$nser)
        colarr<- rain[ser]
    }
    
    if(lines == TRUE) {
        for (i in 1:o.pcaser$nser) {
            x<- o.pcaser$scores[ser==i,1]   ; y<- o.pcaser$scores[ser==i,2]
            lines(x,y,col="gray80",lwd=0.8)
        }
    }
    if(arrows == TRUE) arrows(xfrom,yfrom,xto,yto,col="gray60",lwd=1.5,length=0.10,angle=45)
    points(o.pcaser$scores[,1],o.pcaser$scores[,2],pch=ser,col=colarr,cex=0.6)
    #  if(usecolors == "TRUE") points(o.pcaser$scores[,1],o.pcaser$scores[,2],pch=1,col=colarr,cex=1.5)
    symbols<- as.integer(levels(as.factor(ser)))
    # ------------------------------------------------------------------------------------------------------------------
    legend("topright",levels(o.pcaser$plotlabels),pch=symbols,lty=0,bty="n",ncol=4,x.intersp=0.5,col=legcol,cex=0.6) #
    # ------------------------------------------------------------------------------------------------------------------
}
