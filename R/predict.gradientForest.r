`predict.gradientForest` <-
function (object, newdata, extrap=TRUE, ...)
{
    linfun <- function(xold,yold,xnew) 
        yold[1] + (xnew-xold[1])*diff(yold)/diff(xold)
    if (missing(newdata))
        newdata <- object$X
    if(!inherits(newdata,"data.frame"))
        stop("newdata must be a data.frame")
    newnames <- names(newdata)
    if(!all(ok <- newnames %in% names(object$X))) {
        badnames <- paste(newnames[!ok], collapse=", ")
        stop(paste("the following predictors are not in the gradientForest:\n\t",badnames,sep=""))
    }
    for (varX in newnames) {
        ci <- cumimp(object, varX)
        xold <- range(ci$x)
        yold <- range(ci$y)
        xnew <- range(newdata[,varX],na.rm=T)
        if (extrap)
          ynew <- linfun(xold, yold, xnew)
        else 
          ynew <- yold
        if (xnew[1] < xold[1]) {
            ci$x <- c(xnew[1],ci$x)
            ci$y <- c(ynew[1],ci$y)
        }
        if (xnew[2] > xold[2]) {
            ci$x <- c(ci$x,xnew[2])
            ci$y <- c(ci$y,ynew[2])
        }
        f <- approxfun(ci, rule = 2)  
        newdata[,varX] <- f(newdata[,varX])     
    }
    class(newdata) <- c("predict.gradientForest", "data.frame")
    newdata
}
