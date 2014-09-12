\name{GBRexample}
\alias{GBRexample}
\alias{gf.GBR}
\alias{Phys_site}
\alias{Phys_grid}
\alias{Sp_mat}
\docType{data}
\title{
Great Barrier Reef example data
}
\description{
Biodiversity survey data sets from the far north Great Barrier Reef including
site physical and species data and fine-scale physical data interpolated onto
a grid on latitude-longitude.
}

\usage{
data(GBRexample)
}

\details{
  \describe{
    \item{Phys_site}{
      A data frame with 197 observations on the following 28 variables.
      \describe{
        \item{\code{BATHY}}{Depth from bathymetry }
        \item{\code{SLOPE}}{Slope derived from bathymetry }
        \item{\code{ASPECT}}{Aspect of slope derived from bathymetry }
        \item{\code{BSTRESS}}{Seabed current stress }
        \item{\code{CRBNT}}{Sediment \% carbonate composition}
        \item{\code{GRAVEL}}{Sediment \% gravel grainsize fraction}
        \item{\code{SAND}}{Sediment \% sand grainsize fraction}
        \item{\code{MUD}}{Sediment \% mud grainsize fraction}
        \item{\code{NO3_AV}}{Nitrate bottom water annual average}
        \item{\code{NO3_SR}}{Nitrate seasonal range }
        \item{\code{PO4_AV}}{Phosphate bottom water annual average}
        \item{\code{PO4_SR}}{Phosphate seasonal range}
        \item{\code{O2_AV}}{Oxygen bottom water annual average}
        \item{\code{O2_SR}}{Oxygen seasonal range}
        \item{\code{S_AV}}{Salinity bottom water annual average}
        \item{\code{S_SR}}{Salinity seasonal range}
        \item{\code{T_AV}}{Temperature bottom water annual average}
        \item{\code{T_SR}}{Temperature seasonal range}
        \item{\code{Si_AV}}{Silicate bottom water annual average}
        \item{\code{Si_SR}}{Silicate seasonal range}
        \item{\code{CHLA_AV}}{Chlorophyll annual average }
        \item{\code{CHLA _SR}}{Chlorophyll seasonal range }
        \item{\code{K490_AV}}{Attenuation coefficient at 490nm annual average}
        \item{\code{K490_SR}}{Attenuation coefficient seasonal range}
        \item{\code{SST_AV}}{Sea surface temperature annual average }
        \item{\code{SST_SR}}{Sea surface temperature seasonal range}
        \item{\code{BIR_AV}}{Relative benthic irradiance, annual average}
        \item{\code{BIR_SR}}{Benthic irradiance seasonal range}
      }
    }
    \item{Phys_grid}{
      A data frame with 8682 observations with the following 2 variables as well as 
      the same 28 variables as in \code{Phys_site}.
      \describe{
        \item{\code{NORTH}}{northing in scaled units}
        \item{\code{EAST}}{easting in scaled units}
      }
    }
    \item{Sp_mat}{
      A matrix of 197 rows corresponding to the sites in \code{Phys_site} and 110
      columns corresponding to species. The values are \code{log(y+c)}-transformed 
      species abundance (where \code{c} is the minimum positive abundance \code{y}).
    }
    \item{gf.GBR}{
      A \code{gradientForest} object built from the site data.
    }
  }
}

\references{
Ellis, N., Smith, S.J., and Pitcher, C.R. (2011) Gradient Forests: calculating importance
gradients on physical predictors. \emph{Ecology}, \bold{93}, 156--168.}

\examples{
data(GBRexample)

# transform the predictors using predict() on a fine-scale grid
gf.GBR
predictors <- names(importance(gf.GBR))
gf.pred <- predict(gf.GBR, Phys_grid[,predictors])
plot(gf.pred, Phys_grid[,c("EAST")], Phys_grid[,c("NORTH")], asp=1, palette="gr", pch=15, 
main="Biological composition in Far North Great Barrier Reef")

}
\keyword{datasets}
