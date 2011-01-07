rescale.predictors <- function(obj, predictors, scale) {
  # Multiply predictors by amount scale in X, res$split and dens
  if (!inherits(obj,"gradientForest"))
    stop("obj is not a gradientForest")
  current <- names(obj$X)
  if (!all(m <- is.element(predictors,current))) {
    plural <- sum(!m)>1
    stop(paste("Predictor",if(plural) "s" else ""," ",
      paste(predictors[!m],collapse=",")," ", if(plural) "are" else "is",
      " not in the gradientForest object",sep=""))
  }
  if (length(scale) != length(predictors))
    stop("scale must have same length as predictors")
  names(scale) <- predictors
  for (p in predictors) {
    obj$X[[p]] <- scale[p] * obj$X[[p]]
    obj$dens[[p]]$x <- scale[p]*obj$dens[[p]]$x
    obj$dens[[p]]$y <- (1/abs(scale[p]))*obj$dens[[p]]$y
    obj$res$split[obj$res$var == p] <- scale[p]*obj$res$split[obj$res$var == p]
  }
  obj
}

