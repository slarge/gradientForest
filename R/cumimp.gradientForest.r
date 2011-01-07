`cumimp.gradientForest` <-
function (x, predictor, type=c("Overall","Species")[1], standardize=TRUE, ...)
{
  if (!inherits(x,"gradientForest"))
    stop(paste("'x' must be a gradientForest object"))
  if (length(predictor) != 1)
    stop(paste("'predictor' must be a single string"))
  if (!is.element(predictor,levels(x$res$var)))
    stop(paste("Predictor",predictor,"does not belong to gradientForest object"))
  if (is.na(option <- pmatch(type,c("Overall","Species"))))
    stop(paste('Unmatched type "',type,'". Expecting one of "Overall" or "Species"',sep=""))

  getCUnorm <- function(importance.df) {
    agg <-  with(importance.df, agg.sum(improve.norm,list(split),sort.it=TRUE)) # this can be improve or improve.norm because height gets normalized anyway
    cum.split <- agg[,1]
    height <- agg[,2] * (if (standardize) approx(x$dens[[predictor]]$x, 1/x$dens[[predictor]]$y, cum.split, rule = 2)$y else 1)
    if (any(bad <- is.na(height))) {
      height <- height[!bad]
      cum.split <- cum.split[!bad]
    }
    res <- list(x = cum.split, y = cumsum(height)/sum(height))
  }

  importance.df <- x$res[x$res$var==predictor,]
  if (option == 1) {  # all species combined
    res <- getCUnorm(importance.df)
    res$y <- res$y*importance(x,"Weighted")[predictor]
  } else { # separate species
    res <- tapply(1:nrow(importance.df), importance.df$spec, function(i) getCUnorm(importance.df[i,]))
    res <- lapply(namenames(names(res)), function(species)
      list(x=res[[species]]$x, y=res[[species]]$y * x$imp.rsq[predictor,species]))
  }
  res
}
