`plot.gradientForest` <-
function(x,	plot.type=c("Overall.Importance","Split.Density","Cumulative.Importance","Performance")[1], 
par.args=NULL, plot.args=NULL, ...) 
{
  if (!inherits(x,"gradientForest"))
    stop(paste("'x' must be a gradientForest object"))
  plot.options <- c("Overall.Importance","Split.Density","Cumulative.Importance","Performance")
  if (is.na(plot.option <- pmatch(plot.type,plot.options)))
    stop(paste('Unmatched plot.type "',plot.type,'". Expecting one of "Overall.Importance", "Split.Density", "Cumulative.Importance" or "Performance"',sep=""))

  old.par<-par(no.readonly=TRUE)
  on.exit(par(old.par))

  amend.args <- function(default.args, new.args) {
    # replace those that match
    for(arg in intersect(names(default.args), names(new.args))) 
      default.args[[arg]] <- new.args[[arg]]
    # append those that don't match
    extra <- new.args[is.na(match(names(new.args),names(default.args)))]
    c(default.args,extra)
  }
  
	if(plot.options[plot.option]=="Overall.Importance"){	
    plot.args.def <- amend.args(list(cex.axis = 0.7, cex.names = 0.7, las=1, horiz = TRUE), plot.args)
    plot.args.def<- amend.args(plot.args.def,list(...))     
    par.args.def <- amend.args(list(mfrow = c(1, 2), mar = c(4, 6, 2, 1)), par.args)
    par(par.args.def)    
    do.call("overall.importance.plot",c(list(obj=quote(x)),plot.args.def))
	}
	  
  if(plot.options[plot.option]=="Split.Density"){      
    plot.args.def <- amend.args(list(leg.posn="topright",bin=F, nbin=101, leg.panel=1, barwidth=1, cex.legend=0.8, line.ylab=1.5), plot.args)
    plot.args.def<- amend.args(plot.args.def,list(...))     
    par.args.def <- amend.args(list(mar =c(4.5, 1.5, 0.5, 4.5), omi = c(0.1, 0.25, 0.1, 0.1)), par.args)
    par(par.args.def)    
    do.call("Split.density.plot.method2",c(list(obj=quote(x)),plot.args.def))
  }
  
  if(plot.options[plot.option]=="Cumulative.Importance") {
    plot.args.def <- amend.args(list(leg.posn="topleft",legend=TRUE, common.scale=F, line.ylab=1.0, cex.legend=0.75, show.species=TRUE, show.overall=TRUE, leg.nspecies=10), plot.args)
    plot.args.def<- amend.args(plot.args.def,list(...))     
    par.args.def <- amend.args(list(mar=c(0.0,2.1,1.1,0), omi=c(0.75, 0.75, 0.1, 0.1)), par.args)
    par(par.args.def)    
    do.call("species.cumulative.plot",c(list(obj=quote(x)),plot.args.def))
  }
    
  if(plot.options[plot.option]=="Performance")  {                               
    plot.args.def <- amend.args(list(horizontal = FALSE, show.names = FALSE, las=2, cex.axis = 0.7,cex.labels=0.7,line=2), plot.args)
    plot.args.def<- amend.args(plot.args.def,list(...))     
    par.args.def <- amend.args(list(mfrow=c(1,1)), par.args)
    par(par.args.def)    
    do.call("performance.plot",c(list(obj=quote(x)),plot.args.def))   
	}

  invisible()	

  }
