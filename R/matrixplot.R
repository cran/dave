matrixplot<- function(veg,rmember,use,y=1)
{
    if(use == "columns") vvdat <- t(veg)
    if(use == "rows") vvdat <- veg
    #
    rmember<- as.factor(rmember)
    lev<- levels(rmember)
    #
    svdat<- cor(t(vvdat^y))
    svdat<- (svdat+1)*0.5
    #
    # The same using aggregate()
    #
    com1<- aggregate(svdat,list(rmember),mean)[,-1]
    com<- aggregate(t(com1),list(rmember),mean)[,-1]
    mc<- length(com[,1])
    com<- as.matrix(com)
    o.mxplot<- list(order=mc,mmatrix=com,levels=lev)
}
