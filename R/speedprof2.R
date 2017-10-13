speedprof2<- function(veg,timescale,orders,y=1,adjust=FALSE) {
    deford<- is.null(orders)
    if(deford == TRUE) orders<- c(1,2,3,4)
    maxord<- length(orders)
    defy<- is.null(y)
    if(defy == TRUE) y<- 1
    # adjusting scores to 100% if requested
    if(adjust == TRUE) {
        adj<- function(x) {100*x/sum(x)}
        veg<- t(apply(veg,1,adj))
    }
    # distance matrix of releves, full mode
    mde <- vegdist(veg^y,method = "euclidean",diag=TRUE,upper=TRUE)
    dimde <- dim(veg)[1]                          # matrix dimension
    mde<- as.matrix(mde,dim=c(dimde,dimde))
    # dvec1 is speedprofile of order 1
    dvec1<- diag(mde[2:dimde,1:dimde-1])
    o.speedprof<- list(nrel=dimde,dmatrix=mde,speed1=dvec1,timescale=timescale,orders=orders)
}
