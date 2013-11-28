`importance.combinedGradientForest` <-
function (x, type=c("Weighted","Raw","Species")[1], sort=TRUE, ...)
{
    weighted <- rowSums(x$imp.rsq, na.rm=TRUE)/ncol(x$imp.rsq)
    if (sort)
      o <- order(-weighted)
    else o <- 1:length(weighted)
    nam <- rownames(x$imp.rsq)
    res <- switch(pmatch(type,c("Weighted","Raw","Species")),
      weighted[o],
      rowSums(sweep(x$imp.rsq,2,x$rsq,"/"), na.rm=TRUE)[o]/ncol(x$imp.rsq),
      if (sort) sort(x$rsq,decreasing=T) else x$rsq
    )
    if (is.null(res))
      stop(paste('Unmatched type "',type,'". Expecting one of "Weighted", "Raw" or "Species"',sep=""))
    else res
}
