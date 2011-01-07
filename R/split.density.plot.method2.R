`Split.density.plot.method2` <-
function(obj,imp.vars=NULL, imp.vars.names=imp.vars,
  leg.posn="topright", mfrow=n2mfrow(length(imp.vars)), 
  mar = c(4.5, 1.5, 0.5, 4.5), omi = c(0.1, 0.25, 0.1, 0.1), bin=F, nbin=101, leg.panel=1, barwidth=1,  
  cex.legend=0.8, line.ylab=1.5, ...)
{
if(is.null(imp.vars)) imp.vars<-imp.var.names <- names(sort(rowMeans(obj$imp.rsq),decreasing=T))[1:2]

n2mfrow <- function(n) {
# choose a sensible layout for the plots
  if (length(n) != 1) stop(paste("n must be a single number"))
  if (n==1) return(c(1,1))
  if (n==2) return(c(1,2))
  if (n==3) return(c(2,2))
  n1 <- floor(sqrt(n))
  n2 <- ceiling(n/n1)
  c(n1,n2)
}

is.binned <- function(obj) {
  compact <- obj$call$compact
  if (is.null(compact)) 
    FALSE
  else
    eval(compact)
}
  

normalize.histogram <- function(ci,integral=1,bin=F,nbin=101) {
# scale y values so that histogram integrates to integral
# optionally aggregate the y's into binned x ranges 
  if (bin) {
    brks <- seq(min(ci$x),max(ci$x),len=nbin)
    xx <- cut(ci$x,breaks=brks,inc=T)
    yy <- tapply(ci$y,xx,sum)
    yy[is.na(yy)] <- 0
    ci <- list(x=0.5*(brks[-1]+brks[-nbin]),y=yy)
  }
  dx <- min(diff(ci$x)); 
  Id <- sum(ci$y*dx); 
  ci$y <- ci$y/Id*integral; 
  ci
}

normalize.density <- function(d,integral=1,integrate=T) {
# scale y values so that density integrates to integral
  Id <- if(integrate) integrate.density(d) else 1; 
  d$y <- d$y/Id*integral; 
  d
}

integrate.density <- function(d) {
  integrate(approxfun(d,rule=2),lower=min(d$x),upper=max(d$x))$value
}

scale.density <- function(d,scale=1/mean(d$y)) {
  d$y <- d$y*scale
  d
}

par(mfrow=mfrow)
nice.names <- structure(as.list(imp.vars.names),names=imp.vars)
for (i in imp.vars) {
    imp <- importance(obj)[i]
    resA <- obj$res[obj$res$var == i, ]
    splits <- resA$split
    w <- pmax(resA$improve.norm, 0)
    X <- na.omit(obj$X[,i])
    rX <- range(X)
    dX <- diff(rX)
    
    # importance density I(x)
    dImp <- density(splits, weight = w/sum(w), from = rX[1], to = rX[2])
    if((dX/dImp$bw) > 50)  
      dImp <- density(splits, weight = w/sum(w), from = rX[1], to = rX[2], bw=dX/50)
    dImpNorm <- normalize.density(dImp,imp,integrate=T)
    
    # data density d(x)
    dObs <- density(X, from = rX[1], to = rX[2])
    if((dX/dObs$bw) > 50)  
      dObs <- density(X, from = rX[1], to = rX[2], bw=dX/50)
    dObs <- whiten(dObs,lambda=0.9)
    dObsNorm <- normalize.density(dObs,imp,integrate=T)
    
    # raw importances I
    ci <- cumimp(obj,i,standardize=F)   
    ci$y <- diff(c(0,ci$y))
    ci <- normalize.histogram(ci, imp, bin=bin | !is.binned(obj), nbin=nbin)
    
    # standardized density f(x) = I(x)/d(x)
    dStd <- dImp
    dStd$y <- dImp$y/dObs$y
    dStdNorm <- try(normalize.density(dStd,imp,integrate=T)) # this sometimes does not converge
    if (class(dStdNorm) == "try-error")
      dStdNorm <- normalize.histogram(dStd,imp)
    
    # plot them on same scale
    plot(ci, type='h', col="grey60", xlim=range(splits), lwd=barwidth,
      #ylim = c(0, max(dImpNorm$y, dObsNorm$y, dStdNorm$y, ci$y)), lend=2,
      ylim = c(0, max(dImpNorm$y, dObsNorm$y, dStdNorm$y)*1.1), lend=2,
      xlab = nice.names[[i]], ylab = "", ...)
    lines(dImpNorm, col = "black", lwd = 2)
    lines(dObsNorm, col = "red", lwd = 2)
    lines(dStdNorm, col = "blue", lwd = 2)
    abline(h = mean(dStdNorm$y)/mean(dStd$y), lty = 2, col = "blue")
    if (i == imp.vars[leg.panel])
        legend(leg.posn, legend = c("Density of splits", "Density of data", "Ratio of densities", "Ratio=1"),
        lty = c(1, 1, 1, 2), col = c("black", "red", "blue", "blue"), cex = cex.legend, bty = "n", lwd = 1)
  }
  mtext("Density", side = 2, line=line.ylab, outer=T) 

}
