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
\method{cumimp}{gradientForest}(x, predictor, type = c("Overall", "Species")[1], standardize=TRUE, standardize_after=FALSE, ...)
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
  \item{standardize}{logical, if TRUE then importances are standardized by density}	  
  \item{standardize_after}{logical, if TRUE then standardization, if applied, occurs after R^2 normalization. Default FALSE}	  
  
  \item{...}{further arguments for other methods}
}
\value{
if \code{type} is "Overall", a list with components \code{x} and  \code{y}, 
where \code{x} is the predictor values and  \code{y} is the cumulative importance. 
}
\author{
    N. Ellis, CSIRO, Cleveland, Australia. <Nick.Ellis@csiro.au>.
    S.J. Smith, DFO, Dartmouth, NS, Canada. <Stephen.Smith@dfo-mpo.gc.ca>
}


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

# Demonstrate differences between standardardization before and after normalization
par(mfrow=c(2,1))
plot(density(f1,"A"),main="Density of predictor 'A'",xlab='A')
rug(sample(density(f1,"A")$x,100))

plot(cumimp(f1,"A",standardize=FALSE),type='n',main="",ylab="Cum. importance",xlab='A')
lines(cumimp(f1,"A"),type='l',col="black")
lines(cumimp(f1,"A",standardize_after=TRUE),type='l',col="blue")
lines(cumimp(f1,"A",standardize=FALSE),type='l',col="red")
legend(par("usr")[1],par("usr")[4],legend=c("Standardize then Normalize (default)","Normalize then Standardize","Normalize only"),col=c("black","blue","red"),lty=1,cex=0.8)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
