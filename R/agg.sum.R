`agg.sum` <- function(x, by, sort.it = F)
{
	if(!is.data.frame(by))
		by <- data.frame(by)
	if(sort.it) {
		ord <- do.call("order", unname(by))
		x <- x[ord]
		by <- by[ord,  , drop=F]
	}
	logical.diff <- function(group)
	group[-1] != group[ - length(group)]
	change <- logical.diff(by[[1]])
	for(i in seq(along = by)[-1])
		change <- change | logical.diff(by[[i]])
  by <- by[c(T, change),  , drop = F]
	by$x <- diff(c(0, cumsum(x)[c(change, T)]))
	by
}
