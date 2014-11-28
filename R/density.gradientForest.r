`density.gradientForest` <-
function(x,predictor,...)
{
    if (!inherits(x,"gradientForest"))
      stop(paste("'x' must be a gradientForest object"))
    x$dens[[predictor]]
}
