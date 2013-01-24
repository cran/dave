davesil.default <-
function(ddist,o.hclr,o.relgr,...)  {
     o.sil<- silhouette(o.relgr,ddist)
     o.sumsil<- summary(o.sil)
     names<- as.character(o.hclr$labels)
     names<- strtrim(names,15)
     o.davesil<- list(sil=o.sil,names=names)
     names(as.character(names))
     o.davesil$call<- match.call()
     cat("Call:\n") 
     class(o.davesil) <- "davesil"
     print(o.davesil$call)
     cat("\nAverage group means:\n",round(o.sumsil$clus.avg.widths,digits=3),"\n")
     o.davesil
     }
