#
# Simple utility to add duplicate string values in the names
# Useful with lapply to force the names into the names of the resulting list
#
namenames <- function(x) structure(x,names=x)
