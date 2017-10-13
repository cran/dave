print.outlier<- function(x,...) {
    o.outlier<- x
    star<- " *    "
    space<- "      "
    cat("Outlier Rowname    Nearest          Distance","\n")
    cat("(*)                neighbour","\n")
    lloflist<- o.outlier$olddim[1]
    for (i in 1:lloflist) {
        if(o.outlier$neigh.dist[i] < o.outlier$thresh) {
            cat(sprintf("%-7s %-10s %-10s %12.4f",space,o.outlier$rel.names[i],o.outlier$neigh.names[i],o.outlier$neigh.dist[i]))
            cat("\n")
        } else {
            cat(sprintf("%-7s %-10s %-10s %12.4f",star,o.outlier$rel.names[i],o.outlier$neigh.names[i],o.outlier$neigh.dist[i]))
            cat("\n")
        }
    }
}