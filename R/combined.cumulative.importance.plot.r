combined.cumulative.importance.plot <-
function (obj,weight=c("uniform","species","rsq.total","rsq.mean")[3],use.diff=FALSE, prednames=names(obj$X)[-1], ...)
{
    require(lattice)
     gearnames <- names(obj$dens)[-1]
      CU <- c(
      lapply(prednames, function(predictor) do.call("rbind",lapply(obj$gears[[predictor]], function(gear) {
        cu <- cumimp(obj,predictor,gear=gear)
        if (use.diff) cu$y <- diff(c(0,cu$y))
        data.frame(predictor=predictor,gear=gear,value=cu$x,CU=cu$y)
      }))),
      lapply(prednames, function(predictor) {
        cu <- cumimp(obj,predictor,weight=weight)
        if (use.diff) cu$y <- diff(c(0,cu$y))
        data.frame(predictor=predictor,gear=paste("combined",weight,sep="."),value=cu$x,CU=cu$y)
      })
      )
    CU <- do.call("rbind",CU)
    imp <- importance(obj)
    o <- order(-imp)
    CU$predictor <- ordered(CU$predictor, levels=names(sort(-imp)))
    sps <- trellis.par.get("superpose.line")
    n <- length(levels(CU$gear))
    sps$lwd <- rep(c(1,2),c(n-1,1))
    sps$col <- rainbow(n,end=2/3)
    trellis.par.set("superpose.line",sps)
    print(xyplot(
      CU~value|predictor,
      data=CU,
      groups=gear,
      type='l',
      scales=list(y=list(relation="same"),x=list(relation="free"),cex=0.5),
      par.strip.text=list(cex=0.5),
      ylab=if (use.diff) "Importance" else "Cumulative Importance",
      xlab="Predictor value",
      as.table=T,
      key=list(
        space="top",
        columns=3,
        text=list(levels(CU$gear)),
        lines=list(type='l',col=trellis.par.get("superpose.line")$col[1:n],
        lwd=trellis.par.get("superpose.line")$lwd[1:n])
      )
    ))
}
