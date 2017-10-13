# Fitting a markov chain to a vegetation time series
rfitmarkov <- function(veg,t,adjust=FALSE){
    # adjusting scores to 100% if requested
    x<- veg
    if(adjust == TRUE) {
        adj<- function(x) {x/sum(x)}
        veg<- t(apply(veg,1,adj))
    }
    t <- as.integer(t)
    p <- length(x[1,])        # no. of species
    n <- length(t)            # no. of points in time (data)
    nm <- n-1                 # no. of time intervals (data)
    dd <- dist(t)
    step <- min(dd)           # step length (model)
    if(step == 0) cat("Zero time step encountered!","\n")
    # HERE COMES AN EXIT!
    
    nmodm <- max(dd)/step     # no. of time intervals (model)
    nmod <- nmodm+1           # no. of points in time (model)
    ranget <- max(t)-min(t)   # time range   (data and model)
    tmod <- rep(0.0,nmod)     # time vector (model)
    for (i in 1:nmod) tmod[i] <- t[1]+(i-1)*step
    pt<- array(1:(p*p*n),c(p,p,n))  # 3-dimensional array for n-1 transition matrices
    pt[pt > 0]<- 0
    # Adjust to 100 percent
    yy<-as.numeric(as.matrix(x),nrow=n,ncol=p)
    yy<-matrix(yy,nrow=n,ncol=p)
    for(j in 1:n) yy[j,]<-yy[j,]/sum(yy[j,])
    # derive transition matrices, pt
    for(k in 1:nm){
        dev<-yy[k+1,]-yy[k,]
        for(i in 1:p){
            for(ii in 1:p){
                if(dev[i] < 0) pt[i,ii,k]<-pt[i,ii,k]+(yy[k+1,ii]*abs(dev[i])) # loss
                if(dev[i] > 0) pt[ii,i,k]<-pt[ii,i,k]+(yy[k+1,ii]*abs(dev[i])) # gain
                pt[ii,ii,k] <-yy[k+1,ii]
            }
        }
    }
    for(i in 1:p) for(j in 1:p) pt[i,j,n]<- sum(pt[i,j,])  # sum of transition matrices
    for(j in 1:p) pt[j,,n] <- pt[j,,n]/sum(pt[j,,n])       # normalize transition matrix
    # simulated data in zz
    zz <- rep(0.0,nmod*p)
    zz <- matrix(zz,nrow=nmod,ncol=p)
    zz[1,] <- yy[1,]
    for(k in 1: nmodm) zz[k+1,] <-  zz[k,] %*% pt[,,n]                                          # simulation
    
    # outputlist
    colnames(zz)<- names(x)
    colnames(yy)<- names(x)
    rfitmarkov<- list(fitted.data=zz,raw.data=yy,transition.matrix=pt[,,n],t.measured=t,t.modeled=tmod)
}
