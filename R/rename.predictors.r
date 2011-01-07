`rename.predictors` <- function(obj, old.names, new.names) {
  # Change names in X, overall.imp, overall.imp2, imp.rsq, res$var, res.u$var
  # and dens
  if (!inherits(obj,"gradientForest"))
    stop("obj is not a gradientForest")
  current <- names(obj$X)
  if (!all(m <- is.element(old.names,current))) {
    plural <- sum(!m)>1
    stop(paste("Predictor",if(plural) "s" else ""," ",
      paste(old.names[!m],collapse=",")," ", if(plural) "are" else "is",
      " not in the gradientForest object",sep=""))
  }
  if (length(new.names) != length(old.names))
    stop("new.names must have same length as old.names")
  test <- outer(new.names,new.names,'!=')
  if (!all(test[lower.tri(test)]))
    stop("new.names must all be different")
  names(obj$X)[match(old.names,names(obj$X))] <- new.names
  names(obj$overall.imp)[match(old.names,names(obj$overall.imp))] <- new.names
  names(obj$overall.imp2)[match(old.names,names(obj$overall.imp2))] <- new.names
  rownames(obj$imp.rsq)[match(old.names,rownames(obj$imp.rsq))] <- new.names
  levels(obj$res.u$var)[match(old.names,levels(obj$res.u$var))] <- new.names
  levels(obj$res$var)[match(old.names,levels(obj$res$var))] <- new.names
  names(obj$dens)[match(old.names,names(obj$dens))] <- new.names
  obj
}
