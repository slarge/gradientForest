#
# Functions for binning
# R function density tends to spread bins out too far and can be negative
#
midpoints <- function(x) {
# points between equally spaced points
  0.5*(x[-1]+x[-length(x)])
}
spread <- function(x) {
# output y satisfies midpoints(y) == x
  n <- length(x)
  c(x[1]-(x[2]-x[1])/2,midpoints(x),x[n]+(x[n]-x[n-1])/2)
}
bin <- function(x,nbin) {
  r <- range(x,na.rm=T)
  midbins <- seq(r[1],r[2],len=nbin)
  spread(midbins)
}
#