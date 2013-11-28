`cumimp.gradientForest` <-
function (x, predictor, type=c("Overall","Species")[1], standardize=TRUE, standardize_after=FALSE,
    ...)
{
  if (!inherits(x,"gradientForest"))
    stop(paste("'x' must be a gradientForest object"))
  if (length(predictor) != 1)
    stop(paste("'predictor' must be a single string"))
  if (!is.element(predictor,levels(x$res$var)))
    stop(paste("Predictor",predictor,"does not belong to gradientForest object"))
  if (is.na(option <- pmatch(type,c("Overall","Species"))))
    stop(paste('Unmatched type "',type,'". Expecting one of "Overall" or "Species"',sep=''))
    
  # convert density to its inverse
  inverse <- function(dens) {dens$y <- 1/dens$y; dens}
  
  # crude integral
  crude.integrate <- function(f) sum(f$y)*diff(f$x)[1]
  
  # normalize f(x) to f(x)/fbar
  normalize <- function(f) {
    integral <- try(integrate(approxfun(f,rule=2),lower=min(f$x),upper=max(f$x))$value)
    if (class(integral)=="try-error") integral <- crude.integrate(f)
    f$y <- f$y/integral*diff(range(f$x)); 
    f
  }
     
  getCU <- function(importance.df, Rsq) {
    agg <- with(importance.df, agg.sum(improve.norm, list(split), sort.it=TRUE))
    cum.split <- agg[,1]
    height <- agg[,2] 
    if (standardize & standardize_after) # crucial to normalize this case
      dinv <- normalize(inverse(x$dens[[predictor]]))
    else dinv <- inverse(x$dens[[predictor]]) # 
    dinv.vals <- approx(dinv$x, dinv$y, cum.split, rule=2)$y
    if (any(bad <- is.na(height))) {
      cum.split <- cum.split[!bad]
      height <- height[!bad]
      dinv.vals <- dinv.vals[!bad]
    }
    if (standardize & !standardize_after) height <- height * dinv.vals
    height <- height/sum(height)*Rsq
    if (standardize & standardize_after) height <- height * dinv.vals
    res <- list(x=cum.split, y=cumsum(height))
  }
  
  importance.df <- x$res[x$res$var==predictor,]
  if (option==1) {
    res <- getCU(importance.df, importance(x, "Weighted")[predictor])
  } else {
    species <- names(x$result)
    res <- lapply(namenames(species), function(sp)  
      getCU(subset(importance.df, spec==sp), x$imp.rsq[predictor,sp]))
  }
  res
}