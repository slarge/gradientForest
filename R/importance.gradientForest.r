`importance.gradientForest` <-
function (x, type=c("Accuracy","Impurity","Weighted","Raw","Species")[3], sort=T, ...)
{
    weighted <- rowMeans(x$imp.rsq)
    if (sort)
      o <- order(-weighted)
    else o <- 1:length(weighted)
    nam <- rownames(x$imp.rsq)
    res <- switch(pmatch(type,c("Accuracy","Impurity","Weighted","Raw","Species")),
      x$overall.imp[nam][o],
      x$overall.imp2[nam][o],
      weighted[o],
      rowMeans(sweep(x$imp.rsq,2,x$result,"/"))[o],
      if (sort) sort(x$result,decreasing=T) else x$result
    )
    if (is.null(res))
      stop(paste('Unmatched type "',type,'". Expecting one of "Accuracy", "Impurity", "Weighted", "Raw" or "Species"',sep=""))
    else res
}
