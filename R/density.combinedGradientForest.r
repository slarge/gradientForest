`density.combinedGradientForest` <-
function(x,predictor,gridded=F,gear,...)
{
  if(!gridded) {
    if(missing(gear))
      x$dens$Combined[[predictor]]
    else x$dens[[gear]][[predictor]]
  } else {
    if(missing(gear))
      with(x$CU[[predictor]]$density, list(x=x, y=y[,"Combined"]))
    else with(x$CU[[predictor]]$density, list(x=x, y=y[,gear]))
  }  
}
