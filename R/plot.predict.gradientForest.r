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
    rg=rgb(r=col1, g=col2, b=col3, max=255),
    rb=rgb(r=col1, b=col2, g=col3, max=255),
    gb=rgb(g=col1, b=col2, r=col3, max=255),
    gr=rgb(g=col1, r=col2, b=col3, max=255),
    br=rgb(b=col1, r=col2, g=col3, max=255),
    bg=rgb(b=col1, g=col2, r=col3, max=255)    
    )
    plot(X, Y, col=col, ...)
    invisible()
}
