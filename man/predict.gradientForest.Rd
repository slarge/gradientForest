\name{predict.gradientForest}
\alias{predict.gradientForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict for  gradientForest
}
\description{
Predict method for gradientForest objects.
}
\usage{
\method{predict}{gradientForest}(object, newdata, extrap=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{"gradientForest"} or \code{"combinedGradientForest"}. 
}
  \item{newdata}{
An optional data frame in which to look for variables with which to predict. 
If omitted, the environmental varaibles at the sites are used (\code{object$X}).
}
  \item{extrap}{
if \code{TRUE} use linear extrapolation for predictor values outside the range of the 
original site data, otherwise limit to the range of of the site data.
}
  \item{\dots}{
    further arguments passed to or from other methods.
}
}
\details{
The predictor
cumulative functions can be used to transform grid data layers of environmental variables
to a common biological importance scale. This transformation of the multi-dimensional 
environment space is to a biological space in which coordinate position represents composition
associated with the predictors. These inferred compositional patterns can be mapped in 
biological space and geographic space in a manner analogous to ordination, that takes into account
the non-linear and sometimes threshold changes that occur along gradients.

Where environmental values lie outside the range of the original site data, by default extrapolation 
is performed. That is, if \code{(xmin,xmax)} are the range of the site predictors with corresponding
cumulative importance values \code{(ymin,ymax)}, the prediction \code{y} at a new environmental value 
outside the range \code{(xmin,xmax)} is \code{ymin + (y-ymin)*(x-xmin)/(xmax-xmin)}.
This is equivalent to assigning the average importance inside \code{(xmin,xmax)} to all values
outside the range. If \code{extrap=FALSE}, linear extrapolation is not performed; instead predictions
below \code{xmin} are fixed at \code{ymin} and predictions above \code{xmax} are fixed at \code{ymax}.
This is equivalent to assigning zero importance outside the range of the site data. 
}
\value{
a dataframe in which each predictor has been transformed to the biological scale
by the cumulative importance function, as defined by \code{cumimp}.
}
\references{
Ellis, N., Smith, S.J., and C.R. Pitcher. (2010) Gradient Forests: calculating importance
gradients on physical predictors. (submitted).
}
\author{
N. Ellis, CSIRO, Cleveland, Australia. <Nick.Ellis@csiro.au>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gradientForest}}
}
\examples{
data(CoMLsimulation)
preds <- colnames(Xsimulation)
specs <- colnames(Ysimulation)
f1 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs, ntree=10)
f1.pred<-predict(f1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
