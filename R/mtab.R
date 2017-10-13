mtab <- function(veg,method="raw",y.r=0.5,y.s=0.25,k.r=3,k.s=5,ndiffs=10) {
    # -------------------------------------------------------------------------
    # method (order mode) can be "raw", "sort", "ca", "clust", "aoc", "mulva"
    # Version of 29. 10. 2015, rev. 25. 10. 2015 (method summary)
    # uses functions aoc() and rcentroid()
    #
    # removing empty species vectors to avoid crashes:
    rf<- apply(sign(veg),2,sum)
    veg<- veg[,rf > 0]
    #
    # set minimum group size to 2
    if(k.r <= 1) { k.r<- 2
        cat("WARNING: No. of releve groups increased to 2! \n\n") }
    if(k.s <= 1) { k.s<- 2
        cat("WARNING: No. of species groups increased to 2! \n\n") }
    # preparing outputlist
    transf.r<- y.r
    transf.s<- y.s
    #
    ordermode<- method
    exponent.r<- y.r
    exponent.s<- y.s
    nupperspec<- ndiffs
    shortnames=FALSE
    #
    nrel <- nrow(veg)
    nspec <- ncol(veg)
    #
    # raw table, "raw"
    rorder<- rep(1:nrel,1)
    sorder<- rep(1:nspec,1)
    grr<- rep(1,nrel)           # group lables 1 (case without grouping)
    gss<- rep(1,nspec)          # group lables 1 (case without grouping)
    if (ordermode == "raw"){
        order.rel<- rorder
        order.sp<- sorder
        order.relgr<- grr
        order.spgr<- gss
        MSCC<- NULL
        CAeig<- NULL
        AOCeig<- NULL
    }
    #
    # no more calculations for "raw"!
    if (ordermode != "raw"){
        #
        #    sorted by decreasing frequency, "sort"
        if (ordermode == "sort"){
            Ms<- apply(sign(veg),2,sum)
            sorder<- order(-Ms)
            Mr<- apply(sign(veg),1,sum)
            rorder<- order(-Mr)
            order.rel<- rorder
            order.sp<- sorder
            order.relgr<- grr
            order.spgr<- gss
            MSCC<- NULL
            CAeig<- NULL
            AOCeig<- NULL
        }
        #
        # no more calculations for "sort"!
        if(ordermode != "sort"){
            #
            #       vegetation table with CA, "ca"
            ca<- cca(veg^exponent.r)
            carel<-order(as.array(ca$CA$u[, 1]))
            caspe<-order(as.array(ca$CA$v[, 1]))
            CAeig<- ca$CA$eig/sum(ca$CA$eig)
            if (ordermode == "ca"){
                order.rel<- carel
                order.sp<- caspe
                order.relgr<- grr
                order.spgr<- gss
                MSCC<- NULL
                AOCeig<- NULL
            }
            #
            # no more calculations for "ca"!
            if(ordermode != "ca"){
                #
                #          clustering releves with k.r groups
                mde<- as.dist((1-cor(t(veg^exponent.r)))/2)
                hclust.r <-hclust(mde,method="ward.D2")
                xr<-cutree(hclust.r,k = k.r)
                membership.r <- as.factor(xr)
                
                #          clustering species with k.s groups
                mde <- vegdist(t(veg^exponent.s),method = "manhattan")
                hclust.s <-hclust(mde,method="complete")
                xs<-cutree(hclust.s,k= k.s)
                membership.s <- as.factor(xs)
                if (ordermode == "clust"){
                    order.rel<- hclust.r$order
                    order.sp<- hclust.s$order
                    order.relgr<- membership.r
                    order.spgr<- membership.s
                    MSCC<- NULL
                    AOCeig<- NULL
                }
                #
                # no more calculations for "clust"!
                if(ordermode != "clust"){
                    #
                    #             call of aoc, "aoc"
                    aocout<- aoc(veg,membership.r,membership.s)
                    #
                    #             arranging table according to cluster and aoc
                    #             new releve and species order in newnor and newnos, also used for mulva
                    #             NOTE: initial order is by ca!!!
                    nor<- hclust.r$order
                    nos<- hclust.s$order
                    nor<- carel   # NOTE: initial order would be by ca!!!
                    nos<- caspe   # NOTE: initial order would be by ca!!!
                    nor1.tmp<- as.numeric(membership.r[nor])
                    nos1.tmp<- as.numeric(membership.s[nos])
                    nor1.tmp<- aocout$rgrscores[nor1.tmp,1]
                    nos1.tmp<- aocout$sgrscores[nos1.tmp,1]
                    aocnor<- nor[order(nor1.tmp)]
                    aocnos<- nos[order(nos1.tmp)]
                    #
                    if (ordermode == "aoc"){
                        order.rel<- aocnor
                        order.sp<- aocnos
                        order.relgr<- membership.r
                        order.spgr<- membership.s
                        MSCC<- aocout$MSCC
                        AOCeig<- aocout$eigval/sum(aocout$eigval)
                    }
                    #
                    # no more calculations for "aoc"!
                    if(ordermode != "aoc"){
                        #
                        #                "mulva" starts here
                        #
                        #                aov, used in mulva only
                        ff <- rep(0.0,nspec)
                        pp <- rep(0.0,nspec)
                        for (i in 1:nspec){                         # reserved for Jancey
                            cc<-as.matrix(veg[,i])                   # ff is F-value
                            model<-aov(cc~membership.r)              # pp is p-value
                            ll <- anova(model)
                            ff[i] <- ll[[4]][[1]]
                            pp[i] <- ll[[5]][[1]]
                        }
                        #
                        # the following overwrites Jancey's method by IndVal, not suggested
                        #
                        ##               o.indval<- indval(veg,membership.r)
                        ##               ff<- o.indval$indcls
                        ##               pp<- o.indval$pval
                        #
                        decrspecnam<- names(veg[order(-ff)])      # species names in decreasing order
                        incrpp<- pp[order(-ff)]                   # increasing p-values
                        length(incrpp[incrpp >= 0.05])            # determine discriminating species automatically!
                        # (unused)
                        pslabels<- rep(1:nspec,1)
                        decrspeclab<- pslabels[order(-ff)]        # species numbers, decreasing
                        groupupper<- rep(1,nspec)
                        #                set group number of low resolution species to zero
                        dif<- nspec-nupperspec
                        i1<- nupperspec+1
                        if(dif > 0) groupupper[i1:nspec] <- 0
                        
                        
                        if (ordermode == "mulva") {
                            MSCC<- aocout$MSCC
                            AOCeig<- aocout$eigval/sum(aocout$eigval)
                        }
                        #
                        #                ordering releves within groups according to carel to derive rr
                        #
                        rr<- carel[order(nor1.tmp)]
                        #
                        #                ordering species within groups according to caspe to derive ss
                        #
                        ss<- caspe[order(nos1.tmp)]
                        #
                        #                putting high resolution species on top according to decrspeclab, groupupper
                        current1<- 0
                        current2<- nupperspec
                        ss2<- rep(1,nspec)
                        gss2<- rep(1,nspec)
                        for (i in 1:nspec) for (j in 1:nspec) {
                            if(ss[i] == decrspeclab[j]) {
                                if(groupupper[j] == 1) {
                                    current1 <- current1+1
                                    ss2[current1] <- ss[i]
                                    gss2[current1] <- aocout$new.spgr[i]
                                }
                                if(groupupper[j] == 0) {
                                    current2 <- current2+1
                                    ss2[current2] <- ss[i]
                                    gss2[current2] <- aocout$new.spgr[i]
                                }
                            }
                        }
                        #
                        #                ordering bottom
                        #
                        if(dif > 0) {
                            bottoms<- decrspeclab[i1:nspec]
                            orderbottoms<- bottoms
                            veg4<- as.matrix(veg[,bottoms])
                            if(dif >= 2) for (i in 1:dif) orderbottoms[i]<- sum(sign(veg4[,i]))
                            if(dif == 1) orderbottoms <- sum(sign(veg4))
                            oorderbottoms<- order(-orderbottoms)
                            sfreq<- bottoms
                            ssdown<- ss2[i1:nspec]
                            for (i in 1:dif) for (j in 1:dif){
                                if(ssdown[i] == bottoms[j]) sfreq[i]<- orderbottoms[j]
                            }
                            ssdown<- ssdown[order(-sfreq)]
                            ss2[i1:nspec]<- ssdown
                        }
                        #
                        #                new order of releve and species groups
                        #
                        norg.mu<- membership.r
                        nosg.mu<- membership.s
                        #                lower group to zero
                        if(dif > 0) nosg.mu[bottoms] <- NA
                        if (ordermode == "mulva"){
                            order.rel<- rr
                            order.sp<- ss2
                            order.relgr<- norg.mu
                            order.spgr<- nosg.mu
                        }
                    } # this bracket terminates exclusion of ordermode "aoc"
                } # this bracket terminates exclusion of ordermode "clust"
            } # this bracket terminates exclusion of ordermode "ca"
        } # this bracket terminates exclusion of ordermode "sort"
    } # this bracket terminates exclusion of ordermode "raw"
    # Centroid
    o.c<- rcentroid(veg,order.relgr,y=y.r)
    ord.gr<- order.relgr
    if (ordermode == "aoc") ord.gr<- ord.gr[order(aocout$rgrscores[,1])]
    if (ordermode == "mulva") ord.gr<- ord.gr[order(aocout$rgrscores[,1])]
    if (ordermode == "aoc") ord.gr<- order(aocout$rgrscores[,1])
    if (ordermode == "mulva") ord.gr<- order(aocout$rgrscores[,1])
    ord.spec<- order.sp
    cent<- o.c$prob.table[ord.gr,ord.spec]
    mm<- round(cent,digits=2)
    mm<- matrix(as.character(mm),ncol=ncol(o.c$prob.table))
    lf<- mm == "0"
    mm[lf == TRUE] <- "."
    rownames(mm) <- rownames(o.c$prob.table)[ord.gr]
    colnames(mm) <- colnames(o.c$prob.table)[ord.spec]
    
    o.Mtabs<- list(method=method,transf.r=y.r,transf.s=y.s,order.rel=order.rel,order.sp=order.sp,order.relgr=order.relgr,order.spgr=order.spgr,MSCC=MSCC,CAeig.rel=CAeig,AOCeig.rel=AOCeig,veg=veg,centroids=mm)
}
