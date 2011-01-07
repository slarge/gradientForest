`combined.performance.plot` <-
function(obj,horizontal = FALSE, show.names = FALSE, las = 2, cex.axis = 0.7, ...)
{
     Ylab = expression(R^2)
    Box.temp <- boxplot(split(obj$rsq,factor(rep(names(obj$nspec),obj$nspec))),plot=FALSE)
    Bxp.temp <- bxp(Box.temp, show.names = show.names, horizontal = horizontal, width=obj$nspec, las = las, cex.axis = cex.axis)
    axis(labels=Box.temp$names,side=1+horizontal,at=Bxp.temp,cex.axis=0.45,padj=0,las=2)
    mtext(Ylab,side=2-horizontal,line=2.5)
    title("Overall performance of random forests by gears")
}
