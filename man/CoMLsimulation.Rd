\name{CoMLsimulation}
\alias{CoMLsimulation}
\alias{Xsimulation}
\alias{Ysimulation}
\docType{data}
\title{
Simulated site-species data
}
\description{
Simulated datasets for demonstrating gradient forest method.
}
\usage{
data(CoMLsimulation)
}

\details{
  \describe{
    \item{Xsimulation}{
      A data frame with 100 observations on 10 variables.
      \describe{
        \item{\code{A, B}}{influential variables, each generated uniformly on [0-1]}
        \item{\code{C-J}}{uninfluential variables, each generated uniformly on [0-1]}
      }
    }
  
    \item{Ysimulation}{
      A matrix of simulated species counts on the 100 sites in Xsimulation. The species
      \code{a1-a3} respond to variable \code{A}, \code{b1-b4} respond to variable \code{B} and
      species \code{ab1-ab5} respond jointly to both \code{A} and \code{B}.
    }
  }
  
The species data are generated as Poissons with intensity shaped like normal 
curves along \code{A} and \code{B} gradients.
}


\references{
Ellis, N., Smith, S.J., and Pitcher, C.R. (2012) Gradient Forests: calculating importance
gradients on physical predictors. \emph{Ecology}, \bold{93}, 156--168.}

\examples{
data(CoMLsimulation)
names_a <- paste("a",1:3,sep="")
names_b <- paste("b",1:4,sep="")
par(mfrow=c(2,1),mar=c(3,4,3,1))
matplot(Xsimulation$A,Ysimulation[,names_a],main=substitute(a[1-3]),xlab="A",
ylab="Abundance",axes=FALSE,col=c("black","orange","blue","green"))
mtext("A",side=1,line=1)
axis(2)
box()
matplot(Xsimulation$B,Ysimulation[,names_b],main=substitute(b[1-4]),xlab="B",
ylab="Abundance",axes=FALSE,col=c("black","orange","blue","green"))
mtext("B",side=1,line=1)
axis(2)
box()
}

\keyword{datasets}
