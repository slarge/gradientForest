`importance.combinedGradientForest` <-
function (x, type=c("Weighted","Raw","Species")[1], sort=T, ...)
{
    weighted <- rowMeans(x$imp.rsq,na.rm=T)
    if (sort)
      o <- order(-weighted)
    else o <- 1:length(weighted)
    nam <- rownames(x$imp.rsq)
    res <- switch(pmatch(type,c("Weighted","Raw","Species")),
      weighted[o],
      rowMeans(sweep(x$imp.rsq,2,x$rsq,"/"))[o],
      if (sort) sort(x$rsq,decreasing=T) else x$rsq
    )
    if (is.null(res))
      stop(paste('Unmatched type "',type,'". Expecting one of "Weighted", "Raw" or "Species"',sep=""))
    else res
}
