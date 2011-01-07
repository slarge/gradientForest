`print.gradientForest` <-
function(x,...)
{
    cat("A forest of ",x$ntree," ",x$ranForest.type," trees for each of ",x$species.pos.rsq," species\n", sep = "")
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "\n")
    cat("Important variables:\n")
    nam <- names(importance.gradientForest(x,sort=T))
    print.default(nam[1:min(5,length(nam))],sep=" ",quote=F)
    cat("\n")
    invisible(x)
}
