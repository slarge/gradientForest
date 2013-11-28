`gradientForest` <-
function(data,predictor.vars,response.vars,ntree=10,mtry=NULL, transform=NULL,maxLevel=0,corr.threshold=0.5,compact=FALSE,nbin=101,trace=FALSE)
{
  #Modified 07/10/2009 by S.J. Smith for Nick Ellis' version for either
  #regression or classification trees.

  if(!inherits(data, "data.frame"))
    stop("'data' must be a data.frame")
  X <- data[predictor.vars]
  Y <- data[response.vars]
  if (compact) {
    bins <- do.call("cbind",lapply(X, function(x) bin(x,nbin=nbin))) #Nick Ellis 9/12/2009
  }
  
  if(!is.null(transform))
  {
    Y<-apply(Y,2,transform)  #S.J. Smith 18/01/2010
  } 
  imp <- matrix(0,0,2,dimnames=list(NULL,c("%IncMSE","IncNodePurity")))
  #  fitcmd <- quote(randomForest(Species ~ rhs, data=cbind(Y,X), keep.forest=T, importance=TRUE, na.action=na.omit, keep.inbag=FALSE, ntree=ntree))
  #Conditional permutation version of rF.  Nick Ellis 22/12/2009
  if(is.null(mtry)) fitcmd <- quote(randomForest(Species ~ rhs, data=cbind(Y,X), maxLevel=maxLevel, keep.forest=TRUE, importance=TRUE, ntree=ntree, keep.group=TRUE, keep.inbag=TRUE, corr.threshold=corr.threshold, na.action=na.omit))
  else fitcmd <- quote(randomForest(Species ~ rhs, data=cbind(Y,X), maxLevel=maxLevel, keep.forest=TRUE, importance=TRUE, ntree=ntree, mtry=mtry, keep.group=TRUE, keep.inbag=TRUE, corr.threshold=corr.threshold, na.action=na.omit))
  result <- list()
  species.pos.rsq<-0
  form.rhs<-as.formula(paste("Y ~ ", paste(predictor.vars,collapse = "+"))) #added to replace functions makeForms and plus.  SJ Smith 25/06/2009
  if (trace) {spcount <- 0; cat("Calculating forests for",length(response.vars),"species\n")}
  for (spec in response.vars) {
    if (trace) cat(if((spcount <- spcount+1) %% options("width")$width == 0) "\n." else ".")
    try({
      thisfitcmd <- do.call("substitute",list(fitcmd,list(Species=as.name(spec),SpeciesName=spec,ntree=ntree,rhs=form.rhs[[3]])))
      fit <- eval(thisfitcmd)            
      if (fit$type == "regression") {
        if(!is.na(fit$rsq[fit$ntree])) { 
          if(fit$rsq[fit$ntree] > 0) {
            species.pos.rsq<-species.pos.rsq+1
            if (compact) {
              result[[spec]] <- getSplitImproveCompact(fit,bins)   #added by Nick 06/12/2009
            } else {
              result[[spec]] <- getSplitImprove(fit,X)   #added by Nick 12/05/2009
            }
            imp <- rbind(imp,fit$importance)
          }
        }
      } 
      else if (fit$type == "classification") {
      if(!is.na(fit$err.rate[fit$ntree,"OOB"])){
        # p is proportion of Y on first level (e.g. absent)
        p <- sum(Y[[spec]] == levels(Y[[spec]])[1])/ length(Y[[spec]])
        err0 <-  2*p*(1-p)
        if(fit$err.rate[fit$ntree,"OOB"] < 2*p*(1-p)) {
          species.pos.rsq<-species.pos.rsq+1
          if (compact) {
            result[[spec]] <- getSplitImproveClassCompact(fit,bins,err0)   #added by Nick 10/12/2009
          } else {
            result[[spec]] <- getSplitImproveClass(fit,X,err0)   #added by Nick 24/05/2009
          }
          nclass <- length(levels(Y[[spec]]))
          imp <- rbind(imp,fit$importance[,-(1:nclass)])
        }
        }
      } 
      else stop(paste("unknown randomForest type:",fit$type))
    },silent=FALSE)
  }

  if (!length(result)) {
    warning("No species models provided a positive R^2. \nThe gradient forest is empty")
    return(NULL)
  }
  
  rsq <- sapply(result,function(x) x$rsq[1])
  imp.rsq <- matrix(imp[,1],length(predictor.vars),dimnames=list(predictor.vars,names(result)))
  imp.rsq[imp.rsq<0] <- 0
  imp.rsq <- sweep(imp.rsq,2,colSums(imp.rsq,na.rm=T),"/")
  imp.rsq <- sweep(imp.rsq,2,rsq,"*")
  overall.imp  <- tapply(imp[,1],dimnames(imp)[[1]],mean,na.rm=T)
  overall.imp2 <- tapply(imp[,2],dimnames(imp)[[1]],mean,na.rm=T)  
  out1 <- list(
    X=X,Y=Y,result=result,overall.imp=overall.imp,overall.imp2=overall.imp2,ntree=ntree,
    imp.rsq=imp.rsq,species.pos.rsq=species.pos.rsq,ranForest.type=fit$type
    )
  out2 <- Impurity.based.measures(out1)    
  out1$result <- rsq #modified November 2009, S.J. Smith
  out <- c(out1,out2,call=match.call())
  class(out) <- c("gradientForest","list")
  out
}

