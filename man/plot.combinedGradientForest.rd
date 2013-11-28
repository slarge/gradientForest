\name{plot.combinedGradientForest}
\Rdversion{1.1}
\alias{plot.combinedGradientForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots for combinedGradientForest objects
}
\description{
Plot method for combinedGradientForest objects.
}
\usage{
\method{plot}{combinedGradientForest}(x, plot.type = c("Overall.Importance","Predictor.Ranges","Predictor.Density","Cumulative.Importance","Performance")[1],par.args=NULL,plot.args=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    an object of class "combinedGradientForest" generated by \code{\link{combinedGradientForest}}.
  }


   \item{plot.type}{
    specifies the type of plot defined for the object.  Current choices for
    gradientForest objects are Overall.Importance, Predictor.Ranges, Predictor.Density, Cumulative.Importance
    and Performance. Default is Overall.Importance.
  }

   \item{par.args}{arguments to be passed on to be used by \code{\link{par}} according 
      the plot.type chosen.  See Details for the defaults for each plot type.}
      
  \item{plot.args}{arguments to be passed on according 
      the plot.type chosen.  See Details for the defaults for each plot type.}
  
  \item{\ldots}{further arguments passed to or from other methods.}
  

}
\details{
%%  ~~ If necessary, more details than the description above ~~


The following are the default settings for par.args for each plot type. See  \code{\link{par}} for the definition of each of the arguments.

Overall.Importance: \code{list(mfrow = c(1, 2), mar = c(4, 6, 2, 1))}
  
Predictor.Ranges: None
  
Predictor.Density:  None
       
Cumulative.Importance: None 
 
Performance: \code{list(mfrow=c(1,1),mar=old.mar+c(0,2.5,0,0))}
 

The following are the default settings for plot.args for each plot type.

Overall.Importance: \code{list(cex.axis = 0.7, cex.names = cex.axis, horiz = TRUE, las = 1)}
  
Predictor.Ranges: None
  
Predictor.Density: None
   
Cumulative.Importance: \code{list(weight="rsq.total", use.diff=FALSE, prednames=names(x$X)[-1], show.weights=FALSE, show.gears=TRUE, sort=TRUE)}, where:
 \code{weight} is the type of weighting to perform across gears (see same argument in \code{\link{cumimp.combinedGradientForest}}); 
 if \code{use.diff=TRUE} the differenced cumulative importances are plotted; 
 \code{prednames} is the names of the predictors for which plots are required;
 if \code{show.weights=TRUE} indicate gear weight per bin by colour saturation;
 if \code{show.gears=FALSE} do not show the individual gear cumulative curves;
 and if \code{sort=TRUE}, sort predictors by importance, otherwise use order in \code{prednames}.
 If \code{weight} has multiple elements, the given weightings are shown but not the individual gears.  
                        
Performance: \code{list(horizontal = FALSE, show.names = FALSE, cex.axis = 0.7, las = 2)}, where  
\code{show.names} is set to \code{TRUE} or \code{FALSE} 
to override the defaults on whether an x-axis label on performance plot is printed for each group.

}


\value{ 

The overall importance plot shows a simple barplot of the ranked importances of
the physical variables. The most reliable importances are the \eqn{R^2} weighted importances.

The predictor ranges plot shows box plots of the observed predictors separately for each gear.

The predictor density plot shows the density of the observed predictors with gears denoted
by colour; the combined density is also shown. 

The cumulative importance plot is an integrated form of the split density plot.
The cumulative importance is plotted separately for all species and averaged over all species.
The cumulative importance can be interpreted as a mapping from an environmental gradient on to biological gradient

The performance plot shows the goodness of fit performance measures for all
species for which the physical variables have some predictive power. For
regression, the measure is out-of-bag \eqn{R^2}. For classification, the measure is
out-of-bag error rate.

}
\references{
Breiman, L. (2001) Random Forests. \emph{Machine Learning}, \bold{45(1)}, 5--32.

Ellis, N., Smith, S.J., and Pitcher, C.R. (2012) Gradient Forests: calculating importance
gradients on physical predictors. \emph{Ecology}, \bold{93}, 156--168.

Liaw, A. and Wiener, M. (2002) Classification and regression by randomforest. \emph{R News}, \bold{2(3)},
18--22. \url{http://CRAN.R-project.org/doc/Rnews/}
}

\author{
N. Ellis, CSIRO, Cleveland, Australia. <Nick.Ellis@csiro.au>.
S.J. Smith, DFO, Dartmouth, NS, Canada. <Stephen.Smith@dfo-mpo.gc.ca>

}



\seealso{
\code{\link{plot.gradientForest}}
}
\examples{
data(CoMLsimulation)
preds <- colnames(Xsimulation)
specs <- colnames(Ysimulation)
f1 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6], ntree=10)
f2 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6+6], ntree=10)
f12 <- combinedGradientForest(west=f1,east=f2)
plot(f12,plot.type="Predictor.Ranges")
plot(f12,plot.type="Predictor.Density")
plot(f12,plot.type="Cumulative.Importance")
plot(f12,plot.type="Cumulative.Importance",plot.args=list(weight="uniform"))
plot(f12,plot.type="Cumulative.Importance",plot.args=list(weight="species"))
plot(f12,plot.type="Cumulative.Importance",plot.args=list(weight="rsq.total"))
plot(f12,plot.type="Cumulative.Importance",plot.args=list(weight="rsq.mean"))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
\keyword{methods}% __ONLY ONE__ keyword per line
