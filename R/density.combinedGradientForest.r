`density.combinedGradientForest` <-
function(x,predictor,gridded=F,gf.name,...)
{
  if (!inherits(x,"combinedGradientForest"))
    stop(paste("'x' must be a combinedGradientForest object"))
  if(!gridded) {
    if(missing(gf.name))
      x$dens$Combined[[predictor]]
    else x$dens[[gf.name]][[predictor]]
  } else {
    if(missing(gf.name))
      with(x$CU[[predictor]]$density, list(x=x, y=y[,"Combined"]))
    else with(x$CU[[predictor]]$density, list(x=x, y=y[,gf.name]))
  }  
}
