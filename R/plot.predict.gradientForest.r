`plot.predict.gradientForest` <-
function (x, X, Y, palette="rg", col3=127, ...)
{
    palettes <- c("rg","rb","gr","gb","br","bg")
    if (!(palette %in% palettes)) stop(paste("Unknown palette",palette))    
    
    pc <- prcomp(x)
    
    # set up a colour palette 
    col1 <- pc$x[,1]
    col2 <- pc$x[,2]
    col1 <- (col1-min(col1)) / (max(col1)-min(col1)) * 255
    col2 <- (col2-min(col2)) / (max(col2)-min(col2)) * 255
    
    # plot predicted PC scores
    col <- switch(palette,
    rg=rgb(red=col1, green=col2, blue=col3, maxColorValue=255),
    rb=rgb(red=col1, blue=col2, green=col3, maxColorValue=255),
    gb=rgb(green=col1, blue=col2, red=col3, maxColorValue=255),
    gr=rgb(green=col1, red=col2, blue=col3, maxColorValue=255),
    br=rgb(blue=col1, red=col2, green=col3, maxColorValue=255),
    bg=rgb(blue=col1, green=col2, red=col3, maxColorValue=255)    
    )
    plot(X, Y, col=col, ...)
    invisible()
}
