`print.combinedGradientForest` <-
function(x,...)
{
  cat("\nCall:\n")
  cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = " ")
  cat("\nGears:\n")
  cat(paste(levels(x$X[,1]), sep = " "), "\n", sep = " ")
  cat("\nNumber of Species:\n")
  print(x$nspec)
  cat("\nPredictors:\n")
  cat(paste(names(x$X)[-1], sep = " "), "\n", sep = ", ", fill=T)
  invisible(x)
}