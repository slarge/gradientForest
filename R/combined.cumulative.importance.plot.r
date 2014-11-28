combined.cumulative.importance.plot <-
function (obj,weight=c("uniform","species","rsq.total","rsq.mean","site","site.species","site.rsq.total","site.rsq.mean")[3],
use.diff=FALSE, prednames=names(obj$X)[-1], show.weights=FALSE, show.gf.names=TRUE, sort=TRUE, ...)
{
    if ((nw <- length(weight)) > 1) show.gf.names <- show.weights <- FALSE
    gearnames <- names(obj$dens)[-1]
    CU <- if(!show.gf.names) list() else 
      lapply(prednames, function(predictor) do.call("rbind",lapply(obj$gf.names[[predictor]], function(gear) {
        cu <- cumimp(obj,predictor,gf.name=gear)
        dens <- density(obj,predictor,gf.name=gear,gridded=T)
        if (use.diff) cu$y <- diff(c(0,cu$y))
        data.frame(predictor=predictor,gf.name=gear,value=cu$x,CU=cu$y,dens=dens$y)
      })))      
    CU <- c(CU,
      lapply(prednames, function(predictor) do.call("rbind",lapply(weight, function(w) {
        cu <- cumimp(obj,predictor,weight=w)
        if (use.diff) cu$y <- diff(c(0,cu$y))
        data.frame(predictor=predictor,gf.name=paste("combined",w,sep="."),value=cu$x,CU=cu$y,dens=-1)
      })))
    )
    CU <- do.call("rbind",CU)
    imp <- importance(obj)
    o <- order(-imp)
    CU$predictor <- ordered(CU$predictor, levels=if(sort) names(sort(-imp)) else prednames)
    sps <- trellis.par.get("superpose.line")
    n <- nlevels(CU$gf.name)
    ng <- n - nw
    CU <- transform(CU, maxdens=tapply(dens,interaction(value,predictor),max,na.rm=T)[interaction(value,predictor)])
    CU <- transform(CU, hue=as.numeric(unclass(gf.name))/n, saturation=pmax(dens/maxdens,0.1))
    sps$lwd <- rep(c(3,3),c(ng,nw))
    sps$col <- hsv(h=1:n/n,s=1,v=1:n < n) #rainbow(n,end=1)
    trellis.par.set("superpose.line",sps)
    pgfun <- function(x,y,subscripts,groups,group.value,...){
        n <- length(x)
        panel.segments(x[-1],y[-1],x[-n],y[-n],lwd=with(subset(CU[subscripts,],gf.name==group.value),ifelse(dens != -1,3,1)),
        col=with(subset(CU[subscripts,],gf.name==group.value),hsv(h=hue[-1],s=ifelse(is.na(saturation[-1]),0,saturation[-1]),v=dens != -1)))
      }
    print(xyplot(
      CU~value|predictor,
      data=CU,
      groups=gf.name,      
      type='l',
      scales=list(y=list(relation="same"),x=list(relation="free"),cex=0.5),
      par.strip.text=list(cex=0.5),
      ylab=if (use.diff) "Importance" else "Cumulative Importance",
      xlab="Predictor value",
      as.table=T,
      panel="panel.superpose",
      panel.groups=if(show.weights) pgfun else panel.xyplot,
      key=list(
        space="top",
        columns=min(n,3),
        text=list(levels(CU$gf.name)),
        lines=list(type='l',col=trellis.par.get("superpose.line")$col[1:n],
        lwd=trellis.par.get("superpose.line")$lwd[1:n])
      )
    ))
}
