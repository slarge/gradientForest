#
#   Combine cumulative importances
#   1. interpolate CU to common scale
#   2. difference to get importances
#   3. scale by densities for gear, inverse combined density and normalize
#   4. weighted average across gear (multiple versions of weight)
#   5. accumulate and normalize
#   6. scale to importance
#
#   Weight can be a matrix, with columns corresponding to gear and rows
#   corresponding to type of weighting. If weight is a vector it is
#   converted to a matrix with a single row.
#
combine.cumulative.importance.method1 <- function(CUList, densMat, grid, weight, imp) {
    CUmat <- do.call("cbind", lapply(CUList, "[[", "y"))                  #1
    CUmat <- diff(rbind(0,CUmat))                                         #2
    CUmat <- CUmat*densMat[,-1]/densMat[,1]                               #3
    denom <- colSums(CUmat)                                               #3
    CUmat <- sweep(CUmat,2,denom + (denom==0),"/")                        #3
    if (is.null(dim(weight)))
      dim(weight) <- c(1,length(weight))
    CU <- sweep(array(CUmat,dim=c(dim(CUmat),nrow(weight))),c(3,2),weight,"*")
    CU <- apply(CU,c(1,3),sum)                                            #4
    colnames(CU) <- rownames(weight)
    CU <- apply(CU,2,cumsum)                                              #5
    denom <- CU[nrow(CU),]                                                #5
    CU <- sweep(CU,2,denom + (denom==0),"/")                              #5
    CU*imp                                                                #6
}
