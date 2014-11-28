`cumimp.combinedGradientForest` <-
function (x, predictor, weight=c("uniform","species","rsq.total","rsq.mean","site","site.species","site.rsq.total","site.rsq.mean")[3], gf.name, ...)
{
  if (!inherits(x,"combinedGradientForest"))
    stop(paste("'x' must be a combinedGradientForest object"))
  if (length(predictor) != 1)
    stop(paste("'predictor' must be a single string"))
  if (!is.element(predictor,names(x$X)[-1]))
    stop(paste("Predictor",predictor,"does not belong to combinedGradientForest object"))
  if (is.na(option <- pmatch(weight,c("uniform","species","rsq.total","rsq.mean","site","site.species","site.rsq.total","site.rsq.mean"))))
    stop(paste('Unmatched weight "',weight,'". Expecting one of "uniform", "species", "rsq.total", "rsq.mean", "site", "site.species", "site.rsq.total" or "site.rsq.mean"',sep=""))

  if (missing(gf.name)) {
    res <- x$CU[[predictor]][[paste("combined",weight,sep=".")]]
  } else {
    res <- x$CU[[predictor]][[gf.name]]
  }
  res
}
