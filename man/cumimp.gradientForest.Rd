\name{cumimp.gradientForest}
\Rdversion{1.1}
\alias{cumimp}
\alias{cumimp.gradientForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cumulative Importance of predictors for a gradientForest object
}
\description{
Provide access to cumulative importance curves for a predictor. It is a generic function.
}
\usage{
cumimp(x, ...)
\method{cumimp}{gradientForest}(x, predictor, type = c("Overall", "Species")[1], standardize=TRUE, ...)
}
\arguments{
  \item{x}{
    an object of class "gradientForest", resulting from a call to \code{\link{gradientForest}}.
  }
  \item{predictor}{
    the name of the predictor
  }
  \item{type}{
    indicates the level of detail of the cumulative importance. The choices are
    \itemize{
 	  \item{Overall}{a single overall cumulative importance sequence is returned}
	  \item{Species}{a cumulative importance sequence is returned for each species}
	  }}
  \item{standardize}{logical, if TRUE then...Nick?}	  
  
  \item{...}{further arguments for other methods}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
if \code{type} is "Overall", a list with components \code{x} and  \code{y}, 
where \code{x} is the predictor values and  \code{y} is the cumulative importance. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
    N. Ellis, CSIRO, Cleveland, Australia. <Nick.Ellis@csiro.au>.
    S.J. Smith, DFO, Dartmouth, NS, Canada. <Stephen.Smith@dfo-mpo.gc.ca>
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cumimp.combinedGradientForest}},\code{\link{importance.gradientForest}}
}
\examples{
data(CoMLsimulation)
preds <- colnames(Xsimulation)
specs <- colnames(Ysimulation)
f1 <- gradientForest(data.frame(Ysimulation,Xsimulation), preds, specs[1:3], ntree=10)
par(mfrow=c(2,2))
cu.sp <- cumimp(f1, "A", "Species")
for (sp in names(cu.sp)) 
  plot(cu.sp[[sp]],main=sp,xlab="A",ylab="Cumulative Importance")
plot(cumimp(f1, "A", "Overall"),main="Overall")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
