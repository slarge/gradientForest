predictor.ranges.plot <-
function (obj, ...)
{
    o <- order(-importance(obj))
    X_r <- cbind(obj$X[,1], stack(obj$X[-1]))
    names(X_r) <- c("gear","value","predictor")
    X_r$predictor <- ordered(X_r$predictor, levels=names(sort(-importance(obj))))
    print(bwplot(gear ~ value|predictor, X_r, scales=list(x=list(relation="free",cex=0.5)),
      par.strip.text=list(cex=0.6),xlab="Predictor value", as.table=T, ...))
}
