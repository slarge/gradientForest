`overall.importance.combinedGradientForest.plot` <-
function (obj,las = 1, cex.axis = 0.7, cex.names = cex.axis,  ...)
{

    imp.a <- importance(obj,"Raw")
    imp.w <- importance(obj,"Weighted")
    o.a <- order(imp.a)
    o.w <- order(imp.w)
    barplot(imp.a[o.a], names = names(imp.a[o.a]),
        horiz = TRUE, main = "Raw normalized importance",
        las = las, cex.axis = cex.axis, cex.names = cex.names, ...)
    barplot(imp.w[o.w], names = names(imp.w[o.w]),
        horiz = TRUE, las = 1, main = expression(paste(R^2, " weighted importance")),
        las = las, cex.axis = cex.axis, cex.names = cex.names, ...)
}
