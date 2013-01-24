plot.orank <-
function(x,...) {
   plot(x$x.axis,x$y.axis,asp=1,xlab="",ylab="",axes=FALSE,type="n")
   points(x$x.axis,x$y.axis,ylim=c(0,10),xlim=c(0,10),pch=16,col="black",cex=0.9)
   text(x$x.axis,x$y.axis,x$all.rownam,cex=0.4,col="black",pos=4)
# mark the 5 plots ranked highest
  for(i in 1:x$n.ranks) {
    j<- which(x$all.rownam == x$var.names[i])
    points(x$x.axis[j],x$y.axis[j],pch=16,col="black",cex=1.6)
    text(x$x.axis[j],x$y.axis[j],c(i),cex=1.0,pos=3,offset=0.4)
  }
}
