#    This file creates the simulated dataset that is distributed with the package
#
#    Create some environmental variables A, B, ..., J
#    Environmental matrix (nsite x nX) random uniform data on (minX[1], maxX[1]), ..., (minX[nX], maxX[nX])
#    Run a random forest analysis to get variable importances
#
set.seed(111)
genSpecies <- function(x,m,sd,intens) rpois(x,intens*dnorm(x,m,sd))
genSpecies2D <- function(x1,m1,sd1,x2,m2,sd2,intens) rpois(x1,intens*dnorm(x1,m1,sd1)*dnorm(x2,m2,sd2))
ntree <- 10                # set this to 10 for testing, 500 for proper run
nX <- 10
nsite <- 100
minX <- rep(0,nX)
maxX <- rep(1,nX)
Xsimulation <- as.data.frame(sapply(1:nX, function(i) runif(nsite,min=minX[i],max=maxX[i])))
names(Xsimulation) <- LETTERS[1:nX]
#
#    Species matrix (nsite x nspec) 
#    Create some species that depend on A: a1, a2, a3, ...
#    Create some species that depend on B: b1, b2, b3, ...
#    Create some species that depend on AxB: ab1, ab2, ab3, ...
#
nspec_a <- 3
nspec_b <- 4
nspec_ab <- 5
nspec <- sum(nspec_a,nspec_b,nspec_ab)
# means,sd on environmental gradient
m_a <- seq(minX[1],maxX[1],len=nspec_a+2)[c(1:nspec_a)+1]
m_b <- seq(minX[2],maxX[2],len=nspec_b+2)[c(1:nspec_b)+1]
m_ab <- cbind(
          runif(nspec_ab,min=minX[1],max=maxX[1]),
          runif(nspec_ab,min=minX[2],max=maxX[2])
        )
sd_a <- rep(0.1,nspec_a)
sd_b <- rep(0.1,nspec_b)
sd_ab <- cbind(
          rep(0.1,nspec_ab),
          rep(0.1,nspec_ab)
        )
# Poisson intensity
intens_a <- rep(10,nspec_a)
intens_b <- rep(10,nspec_b)
intens_ab <- rep(10,nspec_ab)
# Species names
names_a <- paste("a",1:nspec_a,sep="")
names_b <- paste("b",1:nspec_b,sep="")
names_ab <- paste("ab",1:nspec_ab,sep="")

# Put the species matrix together
Ysimulation <- data.frame(
    sapply(1:nspec_a, function(i) genSpecies(Xsimulation[,1],m_a[i],sd_a[i],intens_a[i])),
    sapply(1:nspec_b, function(i) genSpecies(Xsimulation[,2],m_b[i],sd_b[i],intens_b[i])),
    sapply(1:nspec_ab, function(i) genSpecies2D(Xsimulation[,1],m_ab[i,1],sd_ab[i,1],Xsimulation[,2],m_ab[i,2],sd_ab[i,2],intens_ab[i]))
)
names(Ysimulation) <- c(names_a,names_b,names_ab)
Ysimulation <- as.matrix(Ysimulation)
head(Ysimulation)
save(Xsimulation, Ysimulation, file="data/CoMLsimulation.rda")
