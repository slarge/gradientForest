`performance.plot` <-
function(obj,filename="rf_overall_performance_GOM.pdf",figure.dir="FiguresGF",
plottofile=FALSE, horizontal = FALSE, show.names = FALSE, las = 2, cex.axis = 0.7, cex.labels = 0.7, line=2, ...)
{
    old.mar<-par()$mar
    par(mfrow=c(1,1),mar=old.mar+c(0,0,0,0))
    if(plottofile) dir.create(figure.dir)
    if(plottofile) pdf(file.path(figure.dir,filename))
    if(obj$ranForest.type=="regression") Ylab <- expression(R^2)
    if(obj$ranForest.type=="classification")  Ylab <- "1-Relative error rate"  #Modified 04/12/2009 S.J. Smith 
    
    perf <- importance(obj, type="Species") 
    n <- length(perf)
    if (horizontal) 
      plot(perf, 1:n, las = las, pch=19, axes=F, xlab="", ylab="", ...)
    else plot(1:n, perf, las = las, pch=19, axes=F, xlab="", ylab="", ...) 
    axis(labels=if (show.names) names(perf) else 1:n,side=1+horizontal,at=1:n, cex.axis=cex.labels, padj=0,las=las) 
    axis(side=2-horizontal, cex.axis=cex.axis)
    if (!show.names)
      mtext("Species performance rank",side=1+horizontal,line=line)   
    mtext(Ylab,side=2-horizontal,line=line)
    title("Overall performance of random forests over species")
    abline(h = 0, lty = 2)
    box()
    if(plottofile) dev.off()
    par(mar=old.mar)
}
