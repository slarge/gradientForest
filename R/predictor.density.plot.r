predictor.density.plot <-
function (obj, ...)
{
    require(lattice)
    bind.varXY <- function(obj) {do.call("rbind", lapply(names(obj), function(predictor) data.frame(predictor=predictor,value=obj[[predictor]]$x,density=obj[[predictor]]$y))) }
    dens <- do.call("rbind", lapply(names(obj$dens), function(gear) cbind(gear=gear,bind.varXY(obj$dens[[gear]]))))
    o <- order(-importance(obj))
    dens$predictor <- ordered(dens$predictor, levels=names(sort(-importance(obj))))
    sps <- trellis.par.get("superpose.line")
    n <- length(levels(dens$gear))
    sps$lwd[1] <- 2
    trellis.par.set("superpose.line",sps)
    print(xyplot(
      density~value|predictor,
      data=dens,
      groups=gear,
      type='l',
      scales=list(relation="free",cex=0.5),
      par.strip.text=list(cex=0.5),
      ylab="Density",
      xlab="Predictor value",
      as.table=T,
      key=list(
        space="top",
        columns=3,
        text=list(levels(dens$gear)),
       lines=list(type='l',col=trellis.par.get("superpose.symbol")$col[1:n],
       lwd=trellis.par.get("superpose.line")$lwd[1:n])
      )
    ))
 }
