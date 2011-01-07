`species.cumulative.plot` <-
function(obj,imp.vars=NULL, imp.vars.names=imp.vars,
    leg.posn="topleft",leg.nspecies=10,legend=TRUE,
    mfrow=rev(n2mfrow(length(imp.vars)*(show.species+show.overall))),
    show.species=TRUE,show.overall=TRUE, mar=c(0.0,2.1,1.1,0), omi=c(0.75, 0.75, 0.1, 0.1), 
    common.scale=F, line.ylab=1.0, cex.legend=0.75, ...)
{
    # May want to allow for transformations
 	  if(is.null(imp.vars)) imp.vars<-imp.var.names <- names(importance(obj))[1:2]

    n2mfrow <- function(n) {
      if (length(n) != 1) stop(paste("n must be a single number"))
      if (n==1) return(c(1,1))
      if (n==2) return(c(1,2))
      if (n==3) return(c(2,2))
      n1 <- floor(sqrt(n))
      n2 <- ceiling(n/n1)
      c(n1,n2)
    }
    
    par(mfrow=mfrow)
    cols <- rainbow(length(levels(obj$res.u$spec))) #modified SJS 08/10/2009
    names(cols) <- levels(obj$res.u$spec)  #modified SJS 08/10/2009
 	  
    xaxt <- if(show.overall) "n" else "s"
    if (show.species) {
      for (varX in imp.vars) {
          CU <- cumimp(obj, varX, "Species")
          xlim <- range(sapply(CU, "[[", "x"))
          ylim <- range(sapply(CU, "[[", "y"))
          plot(xlim,ylim,type='n',xlab=if(show.overall) "" else imp.vars.names[imp.vars==varX],ylab="",xaxt=xaxt,...)
          for(species in names(CU)) {
              isub <- seq(1,length(CU[[species]]$x),len=pmin(500,length(CU[[species]]$x)))
              lines(CU[[species]]$x[isub],CU[[species]]$y[isub],type='s',col=cols[species])
          }
          #added SJS 08/10/2009
          no.species<-length(names(cols))
          # only label most important species
          imp.sp <- sapply(CU, function(cu) max(cu$y))
          best <- order(-imp.sp)[1:min(leg.nspecies,length(imp.sp))]
          if(legend)
              legend(x=leg.posn,legend=names(cols)[best],pch=rep(1,no.species)[best],col=cols[best],bty="n",cex=cex.legend, pt.lwd=2)
      }
    }
    #
    # Combine species
    #
    if (show.overall) {
      for (varX in imp.vars) {
          CU <- cumimp(obj, varX)
          ymax <- max(CU$y)
          if (varX == imp.vars[1]) 
            ymax1 <- ymax
          isub <- seq(1,length(CU$x),len=pmin(500,length(CU$x)))
          plot(CU$x[isub], CU$y[isub], type='s', ylab="",
            xlab=imp.vars.names[imp.vars==varX], 
            ylim=c(0, if(common.scale) ymax1 else ymax), ...)
      }
    }
    mtext("Cumulative importance",side=2,line=line.ylab,outer=TRUE)
}
