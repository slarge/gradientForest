`Impurity.based.measures` <-function(obj)
{
    #becomes an internal function not usually used by users
    #Modified 07/10/2009 by SJS re: NE changes for classification trees.
    dens <- lapply(names(obj$X), function(i) density(na.omit(obj$X[,i]),from=min(na.omit(obj$X[,i])),to=max(na.omit(obj$X[,i]))))
    dens <- lapply(dens,whiten,lambda=0.90) # hard-coded whitening
    names(dens) <- names(obj$X)
    res <- do.call("rbind", lapply(names(obj$result), function(spec) cbind(spec=spec,obj$result[[spec]]))) #added by Smith 13/05/2009
    res$improve <- pmax(0,res$improve)
    res$rsq <- pmax(0,res$rsq)   #added by Ellis 12/05/2009
    res$improve.tot <- tapply(res$improve,res$spec,sum)[res$spec]
    res$improve.tot.var <- tapply(res$improve,interaction(res$spec,res$var),sum)[interaction(res$spec,res$var)]
    res$improve.norm <- with(res,improve/improve.tot*rsq)
    nodup <- !duplicated(res[,1:2])
    res.u <- res[nodup, c("spec","var","rsq","improve.tot","improve.tot.var")]
    res.u$rsq <- with(res.u, ifelse(is.na(rsq), 0, rsq))
    res.u$rsq.var <- with(res.u,rsq*improve.tot.var/improve.tot)
    list(res=res,res.u=res.u,dens=dens)
  }
