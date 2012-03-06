n2mfrow <- function(n) {
  # choose plotting array shape for n panels
  if (length(n) != 1) stop(paste("n must be a single number"))
  if (n==1) return(c(1,1))
  if (n==2) return(c(1,2))
  if (n==3) return(c(2,2))
  n1 <- floor(sqrt(n))
  n2 <- ceiling(n/n1)
  c(n1,n2)
}
