`importance.gradientForest` <-
function (x, type=c("Accuracy","Impurity","Weighted","Raw","Species")[3], sort=TRUE, ...)
{
    if (!inherits(x,"gradientForest"))
      stop(paste("'x' must be a gradientForest object"))
    weighted <- rowSums(x$imp.rsq, na.rm=TRUE)/ncol(x$imp.rsq)
    if (sort)
      o <- order(-weighted)
    else o <- 1:length(weighted)
    nam <- rownames(x$imp.rsq)
    res <- switch(pmatch(type,c("Accuracy","Impurity","Weighted","Raw","Species")),
      x$overall.imp[nam][o],
      x$overall.imp2[nam][o],
      weighted[o],
      rowSums(sweep(x$imp.rsq,2,x$result,"/"), na.rm=TRUE)[o]/ncol(x$imp.rsq),
      if (sort) sort(x$result,decreasing=TRUE) else x$result
    )
    if (is.null(res))
      stop(paste('Unmatched type "',type,'". Expecting one of "Accuracy", "Impurity", "Weighted", "Raw" or "Species"',sep=""))
    else res
}
