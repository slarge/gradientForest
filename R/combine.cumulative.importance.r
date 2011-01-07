#
#   Use weighted average method where weight=density^3 * gear weighting
#
#   Weight can be a matrix, with columns corresponding to gear and rows
#   corresponding to type of weighting. If weight is a vector it is
#   converted to a matrix with a single row.
#
combine.cumulative.importance <- function(FList, densList, grid, weight) {
    
    # linear interpolation to grid
    interpolate <- function(xy)
      approx(xy$x,xy$y,grid,rule=2,method="linear")$y
    
    # convert matrix to a 3D array with n3 copies in the 3rd dimension
    mat2arr <- function(mat,n3) 
      array(mat,dim=c(dim(mat),n3))

    # Ensure weight is a matrix
    if (is.null(dim(weight)))
      dim(weight) <- c(1,length(weight))   
      
    # List of cumulative importances F(x) is on common scale grid
    # Convert to a matrix, one column per gear
    # Then differentiate, converting it to f(x)
    dx <- diff(grid)[1] # assumes grid is uniform
    Fmat <- do.call("cbind", lapply(FList, "[[", "y"))                  
    fmat <- diff(rbind(0,Fmat))/dx
    
    # Interpolate density to common grid
    # densMat is matrix ngrid x number of gears
    # Don't use the combined density, first element of densList
    densMat <- sapply(densList[-1], interpolate)
    
    # Perform the weighted average
    # f[x,type] = sum over gear{ d[x,gear]^3 w[type] f[x,gear]} /
    #             sum over gear{ d[x,gear]^3 w[type] } 
    nweight <- nrow(weight)
    D <- mat2arr(densMat^3, nweight)
    D <- sweep(D,c(3,2),weight,"*")
    fmat <- mat2arr(fmat, nweight)
    fmat_comb <- apply(fmat*D,c(1,3),sum)/apply(D,c(1,3),sum)                      
    
    # Finally integrate f(x) back to F(x)
    colnames(fmat_comb) <- rownames(weight)
    Fmat_comb <- apply(fmat_comb,2,cumsum)*dx                                             
}
