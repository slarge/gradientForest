`getSplitImproveClass` <- 
 function(fit, X, err0)
{
#   return a data-frame: var name, rsq, var number, split value, improvement
  	trees <- lapply(1:fit$ntree, function(k) try(getTree(fit, k),silent=TRUE)) #Nick Ellis 10/12/2009
	  ok <- sapply(trees, class) != "try-error"
 	  tmp <- do.call("rbind", lapply((1:fit$ntree)[ok], function(k) cbind(tree = k, trees[[k]])))
    tmp <- tmp[tmp[,"status"]==1,c("split var","split point","improve")]
    dimnames(tmp) <- list(NULL,c("var_n","split","improve"))
    res<-cbind(data.frame(var=names(X)[tmp[,"var_n"]],rsq=rep((err0-fit$err.rate[fit$ntree,"OOB"])/err0,nrow(tmp))),tmp)
    res
}
