\name{importance.gradientForest}
\Rdversion{1.1}
\alias{importance.gradientForest}
\title{
Importance of the variables in a gradientForest or combinedGradientForest object
}
\description{
Compute the importance measures as a named vector
}
\usage{
\method{importance}{gradientForest}(x, type = c("Accuracy", "Impurity", "Weighted", "Raw", "Species")[3], sort = TRUE, ...)
}
\arguments{
  \item{x}{
    an object of class "gradientForest", resulting from a call to \code{\link{gradientForest}}
  }
  \item{type}{
    the type of importance measure. For \code{type="Species"} the importance measure is the \eqn{R^2} value for each species.
  }
  \item{sort}{
    if \code{TRUE} sort the result in decreasing order
  }
  \item{\dots}{further arguments for other methods}
}
\value{
a named vector of importance measures, one for each variable (or species), possibly sorted
}
\author{
Original code written by N. Ellis, CSIRO, Cleveland, Australia. <Nick.Ellis@csiro.au>. 
        Modified by S.J. Smith, DFO, Dartmouth, NS, Canada. <Stephen.Smith@dfo-mpo.gc.ca>
}

\examples{
data(CoMLsimulation)
preds <- colnames(Xsimulation)
specs <- colnames(Ysimulation)
f1 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6], ntree=10)
f2 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6+6], ntree=10)
f12 <- combinedGradientForest(west=f1,east=f2)
importance(f1)
importance(f1, "Species")
importance(f2, "Impurity")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
