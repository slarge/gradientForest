`getSplitImproveCompact` <- function(fit, bins) {
#   Return a data-frame: var name, rsq, split value, improvement
#   Compact the splits into bins defined by bins matrix
#   The i'th bin for predictor p is the interval (bin[i,p],bin[i+1,p])
#   Every predictor is split into the same number of bins (nrow(bins)-1)

#   extract all trees to a matrix and select for splits with some improvement
  	trees <- lapply(1:fit$ntree, function(k) try(getTree(fit, k),silent=TRUE)) #Nick Ellis 10/12/2009
	  ok <- sapply(trees, class) != "try-error"
  	tmp <- do.call("rbind", lapply((1:fit$ntree)[ok], function(k) cbind(tree = k, trees[[k]])))
    tmp <- tmp[tmp[,"status"]==-3 & zapsmall(tmp[,"improve"]) > 0,c("split var","split point","improve")]
    colnames(tmp) <- c("var_n","split","improve")
    rownames(tmp) <- NULL

#   assign the split to the appropriate bin and aggregate importance in each bin
    Xnames <- colnames(bins)
    tmp <- data.frame(var=Xnames[tmp[,"var_n"]], tmp, bin=rep(0,nrow(tmp)))
    for(p in Xnames) {
        sub <- with(tmp,var==p)
        tmp$bin[sub] <- as.numeric(cut(tmp$split[sub], bins[,p], include=TRUE, ordered=TRUE))
    }
    tmp <- with(tmp[tmp$bin>0,],agg.sum(improve,list(var,bin),sort.it=TRUE))
    names(tmp) <- c("var","bin","improve")
    
#   Set the split value to the bin centre, but retain the bin number in case
#   the bin centre is not appropriate value
    tmp <- cbind(tmp,split=rep(NA,nrow(tmp)),rsq=fit$rsq[fit$ntree])
    midpoints <- function(x) 0.5*(x[-1]+x[-length(x)]) # points between equally spaced points
    for(p in Xnames) {
        sub <- with(tmp,var==p)
        tmp$split[sub] <- midpoints(bins[,p])[tmp$bin[sub]]
    }
    tmp[,c("var","rsq","split","improve","bin")]
}
