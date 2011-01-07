`combinedGradientForest` <-
function(..., nbin=101, method=2)
{
    fList <- list(...)
    ngear <- length(fList)
    if(!all(sapply(fList,inherits,"gradientForest")))
      stop("Every argument must be a gradientForest")
#
#   assign forest names
    if(is.null(gearnames <- names(fList)))
      gearnames <- paste("F",1:ngear,sep="")
    if (any(empty <- gearnames==""))
      gearnames[empty] <- paste("F",1:ngear,sep="")[empty]
    names(fList) <- gearnames
#
#   check the predictor names are the same
    npred <- sapply(fList, function(obj) ncol(obj$X))
#    if(!all(npred == npred[1]))
#      stop("Every forest must have the same number of predictors")
    prednames <- lapply(fList, function(obj) sort(names(obj$X)))  # TO DO: allow for sorting
    allpreds <- unique(sort(unlist(prednames)))
#   find gears that support each predictor
    gears <- lapply(namenames(allpreds), function(predictor) gearnames[sapply(prednames, is.element, el=predictor)])
#    if(!all(prednames == prednames[,1]))
#      stop("Every forest must have the same predictors")
#    prednames <- prednames[,1]
#    npred <- npred[1]
#
#   combined predictor matrix and common bins for importance curve grid
    create.df.aux <- function(X) as.data.frame(do.call("cbind",lapply(namenames(allpreds), function(pred) {res <- X[[pred]]; if(is.null(res)) rep(NA,nrow(X)) else res})))
    create.df <- function(X,transpose=F) {
      if (transpose) X <- as.data.frame(t(X))
      X <- create.df.aux(X)
      if (transpose) X <- as.data.frame(t(X))
      X
    }

    X <- do.call("rbind",lapply(gearnames, function(a) {cbind(gear=a,create.df(fList[[a]]$X))}))
#    X <- do.call("rbind",lapply(gearnames, function(a) {cbind(gear=a,fList[[a]]$X)}))
    bins <- do.call("cbind",lapply(X[allpreds], function(x) bin(x,nbin=nbin)))
    imp.rsq.list <- lapply(fList, function(x) create.df(x$imp.rsq,transpose=T))
    imp.rsq <- do.call("cbind",imp.rsq.list)
    rsq <- unlist(lapply(fList, function(x) x$result))
#
#   combined density calculation
    X_r <- cbind(X[,1], stack(X[allpreds]))
    names(X_r) <- c("gear","value","predictor")
    nspec <- sapply(fList,"[[","species.pos.rsq")
    X_r$nspec <- nspec[X_r$gear]
    X_r <- na.omit(X_r)
    dens <- with(X_r,tapply(1:nrow(X_r),predictor,function(sub) {
      whiten(density(value[sub],weight=nspec[sub]/sum(nspec[sub])),lambda=0.95)
    }))
    dens <- c(list(Combined=dens),lapply(fList, function(x) x$dens))
#
#   Gather the overall cumulative importances from each gradientForest
#   Combine cumulative importances
#   Normalize relative to combined importance
#
    gridded.cumulative.importance <- function(obj, predictor) {
      cu <- cumimp(obj, predictor=predictor)
      grid <- bins[,predictor]
      y <- approx(cu$x,cu$y,grid,rule=2,method="linear")$y
      list(x=grid, y=y)
    }
    CU <- lapply(namenames(allpreds), function(predictor)
      lapply(fList[gears[[predictor]]], gridded.cumulative.importance, predictor=predictor))
    rsq.total <- sapply(lapply(fList,"[[","result"),sum)
    imp.rsq.total <- sapply(imp.rsq.list,rowSums)
    for (predictor in allpreds) {
      g <- gears[[predictor]]
      weight <- rbind(uniform=rep(1,length(g)), species=nspec[g], rsq.total=imp.rsq.total[predictor,g],rsq.mean=imp.rsq.total[predictor,g]/nspec[g])
      densList <- lapply(dens[c("Combined",g)],"[[",predictor) # list of densities, combined version first
      grid <- bins[,predictor]
      if (method==2) {
        CUmat <- combine.cumulative.importance(CU[[predictor]][g], densList, grid, weight)
      } else if (method==1) {
        imp <- rowMeans(imp.rsq)[predictor]
        CUmat <- combine.cumulative.importance.method1(CU[[predictor]][g], densList, grid, weight, imp)
      } else stop(paste("Unknown method:",method))
      for (i in rownames(weight))
        CU[[predictor]][[paste("combined",i,sep=".")]] <- list(x=grid,y=CUmat[,i])
    }

    out <- list(
      call = match.call(),
      X = X,
      dens = dens,
      imp.rsq = imp.rsq,
      rsq = rsq,
      nspec = nspec,
      CU = CU,
      gears = gears
      )
    class(out) <- c("combinedGradientForest","list")
    out
  }

