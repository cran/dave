plot.SNPsm<- function(x,...,out.seq=1,col=FALSE) {
    o.SNPsm<- x
    imax<- o.SNPsm$imax
    jmax<- o.SNPsm$jmax
    x<- o.SNPsm$sim.data
    tmap<- o.SNPsm$tmap
    igmap<- o.SNPsm$igmap # map of initial conditions
    cgmap<- matrix(as.character(igmap),nrow=imax)
    nt<- o.SNPsm$n.time.steps
    vegdef<- o.SNPsm$vegdef
    tsl<- o.SNPsm$time.step.length      # time step length
    frame<- o.SNPsm$frame
    out.seq<- max(out.seq,tsl)          # output sequence not shorter than tsl
    p.out<- as.integer(out.seq/tsl)     # output interval in time step
    #
    #  print map of initial conditions, igmap, rev. 28.7.2016
    #
    vegtypes<- c("1 Pinus","2 Carex","3 Festuca","4 Deschampsia","5 Trisetum","6 Aconitum")
    # iv and jv are the x- and y-coord. in the map (cgmap)
    # reverse axes dimensions first!
    jmax1<- o.SNPsm$imax
    imax1<- o.SNPsm$jmax
    colors<- c(gray(0.85),gray(0.2),gray(0.45),gray(0.6),gray(0.7),gray(0.8))
    if(col==TRUE)colors<- c("darkolivegreen4","lightgreen","gold","darkorange","red1","darkred")
    par(mfrow=c(1,1),mar=c(0,0,0,0),oma=c(2,2,2,2))
    iv<- rep(1:imax1,rep(jmax1,imax1))
    jv<- rep(seq(jmax1,1,-1),imax1)
    plot(iv,jv,type="n",asp=1,axes=FALSE,xlab="",ylab="")
    text(iv,jv,as.vector(cgmap),col=colors[as.vector(igmap)],cex=0.8,font=2)
    points(21,25,pch=1,cex=2.0,col="black") # This is the hut!
    fra<- as.vector(frame)
    fra[fra == 0]<- NA
    fra[fra == 1]<- 0
    points(iv,jv,cex=1.6,col=gray(0.8),pch=fra*22)
    legend("right",c(1,2,3,4,5,6),vegtypes,cex=0.6,col=colors,bty="n",pch=15,text.font=3)
    
    
    #
    #  Plot discrete map of state at t=1, using correlation
    #
    par(mfrow=c(4,6),mar=c(0,0.25,0,0.25),omi=c(0,0,0,0),lwd=0.4)
    for (i in 1:imax) for (j in 1:jmax) {
        tmap[i,j]<- which.max(cor(vegdef,x[i,j,1,1:6]))
    }
    plot(iv,jv,type="n",asp=1,axes=FALSE,xlab="",ylab="")
    points(iv,jv,cex=1.8,col=colors[as.vector(tmap)],pch=15)
    legend("top","t= 0",xjust=0.5,box.lty=0,text.col="black",inset=c(0,-0.04))
    #
    # All following maps
    #
    for (t in 1:nt) {
        if(t%%p.out == 0) {
            #          cat("Output time =",t*tsl,"\n")
            for (i in 1:imax) for (j in 1:jmax) {
                tmap[i,j]<- which.max(cor(vegdef,x[i,j,t,1:6]))
            }
            plot(iv,jv,type="n",asp=1,axes=FALSE,xlab="",ylab="")
            points(iv,jv,cex=1.8,col=colors[as.vector(tmap)],pch=15)
            legend("top",paste("t=",t*tsl),xjust=0.5,box.lty=0,text.col="black",inset=c(0,-0.04))
        } 
    }
}
