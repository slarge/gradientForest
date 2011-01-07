`cumimp.combinedGradientForest` <-
function (x, predictor, weight=c("uniform","species","rsq.total","rsq.mean")[3], gear, ...)
{
  if (!inherits(x,"combinedGradientForest"))
    stop(paste("'x' must be a gradientForest object"))
  if (length(predictor) != 1)
    stop(paste("'predictor' must be a single string"))
  if (!is.element(predictor,names(x$X)[-1]))
    stop(paste("Predictor",predictor,"does not belong to combinedGradientForest object"))
  if (is.na(option <- pmatch(weight,c("uniform","species","rsq.total","rsq.mean"))))
    stop(paste('Unmatched weight "',weight,'". Expecting one of "uniform", "species", "rsq.total" or "rsq.mean"',sep=""))

  if (missing(gear)) {
    res <- x$CU[[predictor]][[paste("combined",weight,sep=".")]]
  } else {
    res <- x$CU[[predictor]][[gear]]
  }
  res
}
