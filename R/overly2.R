# Function overly, S3 class with plot method (10)
# ===============================================
# overlay of time series based on similarity  09.05.2014
# rev. 20. 7. 2016, 10. 8. 2017, widths activated: 14.9.17
# Code simplified: 9. 10. 2017, alingn and cout chanted: 11. 10. 2017

overly<- function(veg,Plot.no,y,sint,...) UseMethod("overly")
overly.default<- function(veg,Plot.no,y,sint,...) {
    o.overly<- overly2(veg,Plot.no,y,sint)
    o.overly$call<- match.call()
    cat("Call:\n")
    class(o.overly) <- "overly"
    print(o.overly$call)
    o.overly
    nt<- o.overly$n.tsteps
    cat("Number of time steps in new time series:  ",nt,"\n")
    cat("Time span of the new time series:          0 -",o.overly$tsteps[nt],"\n")
    o.overly
}
#
# plot method for overly, ordination and minspan
# ----------------------------------------------
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


overly2<- function(veg,Plot.no,y,sint) {
    # -------------------------------------
    # sint: sampling interval. Code simplified: 9. 10. 17
    # align and count reduced to 2 and 1 dimension, 11.10.17
    vegtypes<- names(veg)
    veg<- veg^y
    veg<- veg[order(Plot.no),]                        # rearranging the series
    ser<- as.integer(table(Plot.no))                  # number of releves per time series
    lev<- levels(as.factor(Plot.no))                  # original names of plots, used for legends
    nt<- length(ser)                                  # no. of time series
    dser<- rep(0,nt*nt)                               # distance matrix of time series
    dim(dser)<-c(nt,nt)
    mser<- rep(0,nt*nt*2)                             # labels of closest observations
    dim(mser)<-c(nt,nt,2)
    kids<-rep(0,nt)
    jj<-rep(0,2)
    drel<- as.matrix(dist(veg))                       # drel is the full distance matrix (all releves)
    nspec <- length(veg[1,])                          # number of species
    #
    # distance matrix of series, processing col by col (i), while row is (j)
    # notation for the subset of the full distance matrix is drel[i1:i2,j1:j2]
    #
    i1<- 0 ; i2<- 0
    for (i in 1:nt){
        i1<- i2+1
        i2<- i2+ser[i]
        j1<- 0 ; j2<- 0
        for (j in 1:nt) {
            j1<- j2+1
            j2<- j2+ser[j]
            if (i != j) {
                m<- min(drel[i1:i2,j1:j2])
                jj<- which(drel[i1:i2,j1:j2] == m)
                dser[j,i]<- m                           # dser: distance matrix of time series
                mser[j,i,2]<- ceiling(jj[1]/ser[i])
                mser[j,i,1]<- jj[1]-((ser[i]*(mser[j,i,2]-1)))
            }
        }
    }
    out <- pco(dser,k=2)                             # This is PCOA
    tree<- spantree(dser)                            # minimum spanning tree
    # shifting time series into proper position
    kids[2:nt]<- tree$kid    ; kids[1]<-kids[2]
    parents<- c(1:nt)
    merged<- rep(0,nt) ; merged[1]<- 1
    shift<- rep(0,nt)
    shift[parents[2]]<- mser[kids[2],parents[2],1]-mser[kids[2],parents[2],2]+shift[kids[2]]
    merged[2]<- 1
    while(sum(merged) < nt) {
        for (i in 2:nt) {
            if (merged[kids[i]]==1 & merged[parents[i]]==0) {
                shift[parents[i]]<- mser[kids[i],parents[i],2]-mser[kids[i],parents[i],1]+shift[kids[i]]
                merged[parents[i]]<- 1
            }
        }
    }
    shift<- shift-min(shift)
    range<-max(shift+ser)
    # back-transform veg
    veg<- veg^(1/y)
    # linex1, linex2 and ltext are for the second plot
    linex1<- rep(0,nt)
    linex2<- rep(0,nt)
    ltext<- rep("x",nt)
    is<- 1
    #
    ## assessing align and count
    ## align.null<- rep(0,range*nspec)
    ## all matrices coerced to double
    ## align.null<- matrix(as.double(align.null),nrow=range)
    ## veg<- matrix(as.double(veg),ncol=nspec)
    ## align.temp<- align.null
    ## align.sum<- align.null
    count.null<- rep(0,range)
    count.temp<- count.null
    count.sum<- count.null
    #
    nrel<- nrow(veg)
    itim<- rep(0,nrel)
    is<-1
    for(i in 1:nt) {
        #   solution with aggregate
        l<- is:(is+ser[i]-1)
        m<- (shift[i]+1):(shift[i]+ser[i])
        itim[l]<- m
        ##    myidx2<- (shift[i]+1):(shift[i]+ser[i])
        ##    myidx3<- 1:nspec
        ##    myidx4<- is:(is+ser[i]-1)
        ##    align.temp[myidx2,myidx3]<- veg[myidx4,myidx3]
        
        is<-is+ser[i]
        # ---- new count ----
        count.temp[(shift[i]+1):(shift[i]+ser[i])]<- rep(1,ser[i])
        linex1[i]<- shift[i]+1
        linex2[i]<- shift[i]+ser[i]
        ltext[i]<- lev[i]
        ##    align.sum<- matrix(as.numeric(align.sum) + as.numeric(align.temp),nrow=range)
        ##    align.sum<- align.sum + align.temp
        ##    align.temp<- align.null
        count.sum<- count.sum+count.temp
        count.temp<- count.null
    }
    ##        cat("itim",itim,"\n")
    agg<-          aggregate(veg,list(itim),sum)
    S<- agg[,-1]
    # S<- align.sum
    C<- count.sum
    M<- S/C
    timescal<- seq(0,(range-1)*sint,sint)
    # The output list
    M<- as.data.frame(M)
    colnames(M) <- vegtypes
    rownames(M) <- as.character(seq(1,range,1))
    overly<- list(plot.labels=levels(Plot.no),n.tsteps=range,tsteps=timescal,tser.data=M,ord.scores=out$points,d.mat=dser,tree=tree,vegraw=veg,linex1=linex1,linex2=linex2,ltext=ltext,sint=sint,vegtypes=vegtypes)
}