#
#   Combine cumulative importances
#     Convert list of cumulative importance (F(x)) to matrix
#     Differentiate to get importances (f(x))
#     Multiply by density to get back to raw importances (I(x))
#     Estimate combined importance (f(x)) by weighted least-squares:
#             I(x) = f(x)d(x) + epsilon, weight = d(x)*w(gear)
#
#   Weight can be a matrix, with columns corresponding to gear and rows
#   corresponding to type of weighting. If weight is a vector it is
#   converted to a matrix with a single row.
#
#   This version is slow because it uses lm. The version combine.cumulative.importance
#   uses raw matrix operations and so is much more efficient.
#
combine.cumulative.importance.lm <- function(CUList, densList, grid, weight) {

    # Ensure weight is a matrix
    if (is.null(dim(weight)))
      dim(weight) <- c(1,length(weight))
      
    # List of cumulative importances F(x) is on common scale grid
    # Convert to a matrix, one column per gear
    # Then differentiate, converting it to f(x)
    dx <- diff(grid)[1] # assumes grid is uniform
    CUmat <- do.call("cbind", lapply(CUList, "[[", "y"))                  
    CUmat <- diff(rbind(0,CUmat))/dx
                                          
    # Interpolate density to common grid
    # densMat is matrix ngrid x number of gears+1
    # First column is combined density, drop it
    interpolate <- function(xy, grid)
      approx(xy$x,xy$y,grid,rule=2,method="linear")$y
    densMat <- sapply(densList, interpolate, grid=grid)
    densMat <- densMat[,-1,drop=F]
    
    # Convert importances back to raw importances
    CUmat <- CUmat*densMat
    
    # Create a data frame with grid, importance, density, weight
    ngear <- ncol(CUmat)
    ngrid <- length(grid)
    nweight <- nrow(weight)
    df <- data.frame(
      grid=rep(grid,ngear), 
      gear=rep(1:ngear,each=ngrid),
      importance=as.vector(CUmat), 
      density=as.vector(densMat)
    )[rep(1:(ngrid*ngear),nrow(weight)),]
    df$weight <- rep(as.vector(t(weight)),each=ngrid)
    df$weightrow <- rep(1:nweight,each=ngrid*ngear)
    
    # Prepare predicted importance matrix, one column per weighting
    CU <- matrix(0,ngrid,nweight,dimnames=list(NULL,rownames(weight)))
    
    # Fit using weighted least squares
    # We could estimate standard errors too
    for (w in 1:nweight) {   
      fit <- lm(importance ~ density:factor(grid) - 1, df, weights=density*weight, subset=weightrow==w)
      X <- model.matrix(~ factor(grid) - 1, df)
      se <- sqrt(diag(X %*% vcov(fit) %*% t(X)))
      CU[,w] <- predict(fit,newdata=subset(transform(df,density=1),gear==1 & weightrow==1))
    }
    
    # Finally integrate f(x) back to F(x)
    CU <- apply(CU,2,cumsum)*dx                                              #5
}
