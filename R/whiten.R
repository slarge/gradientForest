`whiten` <-
function(dens, lambda=0.9)
{
    # add a small uniform value to the density to avoid zeroes when taking inverse
    dens$y <- lambda*dens$y + (1-lambda)/diff(range(dens$x))
    dens
}