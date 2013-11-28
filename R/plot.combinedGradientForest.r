`plot.combinedGradientForest` <-
function (x, plot.type = c("Overall.Importance","Predictor.Ranges",
        "Predictor.Density","Cumulative.Importance","Performance")[1],
        par.args=NULL,plot.args=NULL,...)
         
{   plot.options <- c("Overall.Importance","Predictor.Ranges","Predictor.Density","Cumulative.Importance","Performance")
  if (is.na(plot.option <- pmatch(plot.type,plot.options)))
    stop(paste('Unmatched plot.type "',plot.type,'". Expecting one of "Overall.Importance", "Predictor.Ranges", "Predictor.Density", "Cumulative.Importance" or "Performance")',sep=""))

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
    plot.args.def <- amend.args(list(las = 1, cex.axis = 0.7, cex.names = 0.7), plot.args)
    plot.args.def<- amend.args(plot.args.def,list(...))     
    par.args.def <- amend.args(list(mfrow = c(1, 2), mar = c(4, 6, 2, 1)), par.args)
    par(par.args.def)    
    do.call("overall.importance.combinedGradientForest.plot",c(list(obj=quote(x)),plot.args.def))
	}
    
            
 if(plot.options[plot.option]=="Predictor.Ranges"){	
    plot.args.def<- amend.args(plot.args,list(...))     
    if(!is.null(par.args)) par(par.args)
    do.call("predictor.ranges.plot",c(list(obj=quote(x)),plot.args.def))
	}
              
     
    
 if(plot.options[plot.option]=="Predictor.Density"){	
    plot.args.def<- amend.args(plot.args,list(...))     
    if(!is.null(par.args)) par(par.args)  
    do.call("predictor.density.plot",c(list(obj=quote(x)),plot.args.def))
	}
  
    
                    
 if(plot.options[plot.option]=="Cumulative.Importance"){	
    plot.args.def <- amend.args(list(weight="rsq.total", use.diff=FALSE, prednames=names(x$X)[-1], 
      show.weights=FALSE, show.gears=TRUE, sort=TRUE), plot.args)
    plot.args.def<- amend.args(plot.args.def,list(...))     
    if(!is.null(par.args)) par(par.args)
    do.call("combined.cumulative.importance.plot",c(list(obj=quote(x)),plot.args.def)) 
	}
        
 if(plot.options[plot.option]=="Performance"){	
    plot.args.def <- amend.args(list(horizontal = FALSE, show.names = FALSE, las = 2, cex.axis = 0.7), plot.args)
    plot.args.def<- amend.args(plot.args.def,list(...))  
    old.mar<-par()$mar   
    par.args.def <- amend.args(list(mfrow=c(1,1),mar=old.mar+c(0,2.5,0,0)), par.args)
    par(par.args.def)    
    do.call("combined.performance.plot",c(list(obj=quote(x)),plot.args.def))
    par(mar=old.mar)
	}         
             
     invisible()
}
