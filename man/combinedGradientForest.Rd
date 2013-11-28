\name{combinedGradientForest}
\Rdversion{1.1}
\alias{combinedGradientForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combine gradientForest objects
}
\description{
Creates an object of class \code{combinedGradientForest} that represents the synthesis of
two or more \code{gradientForest} objects.
}
\usage{
combinedGradientForest(..., nbin = 101, method=2, standardize=c("before","after")[1])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{any number of gradientForest objects}
  \item{nbin}{number of bins for common predictor grid.  Default set to 101.
}
  \item{method}{  method=1, calls combine.cumulative.importance.method1, method=2 calls
  combine.cumulative.importance.
}
  \item{standardize}{ Should standardization by density occur before or after normalization to R^2?
  Takes values "before" or "after", or abbreviations thereof. Default "before".
}
}

\value{
    \item{call}{
        the matched call
    }
    \item{X}{
        combined data.frame of predictor variables with first column denoting
        the name of the source gradientForest
    }
    \item{dens}{
        list of lists of Gaussian kernel density estimates for each physical
        variable and source.
    }
    \item{rsq}{
        a named vector of species \eqn{R^2} for those species for which the
        physical variables have some predictive power.
        See \code{\link{gradientForest}} for details.
    }
    \item{imp.rsq}{
        a matrix of importance values for predictor and species. The columns sum
        to species \eqn{R^2}.
    }
    \item{nspec}{
        a named vector of number of species for which the physical variables
        have some predictive power.
    }
    \item{CU}{
        list of lists of cumulative importance for each predictor and source.
        Also holds the combined cumulative importance and the gridded density per predictor.
    }
    \item{gears}{
        list of gears having information on each predictor.
    }
}
\references{
Breiman, L. (2001) Random Forests, \emph{Machine Learning}, \bold{45(1)}, 5--32.

Ellis, N., Smith, S.J., and Pitcher, C.R. (2012) Gradient Forests: calculating importance
gradients on physical predictors. \emph{Ecology}, \bold{93}, 156--168.

Liaw, A. and Wiener, M. (2002) Classification and regression by randomforest. \emph{R News}, \bold{2(3)},
18--22. \url{http://CRAN.R-project.org/doc/Rnews/}

Strobl, C. Boulesteix, A.-L., Kneib, T., Augustin, T. and  Zeilis, A. (2008) Conditional variable
importance for random forests. \emph{BMC Bioinformatics}, \bold{9}, 307--317.
Open Access: \url{http://www.biomedcentral.com/1471-2105/9/307}
}
\author{
    N. Ellis, CSIRO, Cleveland, Australia. <Nick.Ellis@csiro.au>.
    S.J. Smith, DFO, Dartmouth, NS, Canada. <Stephen.Smith@dfo-mpo.gc.ca>
}


\examples{
data(CoMLsimulation)
preds <- colnames(Xsimulation)
specs <- colnames(Ysimulation)
f1 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6], ntree=10)
f2 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:6+6], ntree=10)
f12 <- combinedGradientForest(west=f1,east=f2)
plot(f12,plot.type="Predictor.Ranges")
}
