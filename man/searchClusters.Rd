\name{searchClusters}
\alias{searchClusters}

\title{Search clusters in the result grid of species richness estimation.}
\description{This routine searches clusters in the result grid of species richness estimation.}
\usage{searchClusters(species.richness, dimension, origin, resolution, clusterlimit)}
\arguments{
  \item{species.richness}{The grid that should be adjusted. It contains the result of species richness estimation or cross-validation.}
  \item{dimension}{The dimension of the processed grid.}
  \item{origin}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{clusterlimit}{The limit under which values of 'species.richness' should not be used to build clusters.}
}
\details{This routine searches clusters in the result grid of species richness estimation to find centers of species richness.}
\value{This function returns a list containing vectors of the position of pixels which belong together.}
\references{Raedig, C., Dorman, C.F., Hildebrandt, A. and Lautenbach, S. (2010). 
	Reassessing Neotropical angiosperm distribution patterns based on 
	monographic data: a geometric interpolation approach. Biodivers Conserv, 19, 1523-1546.
}
\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
data(dataset.all.species)
data(dataset.landwater)

##create grid parameters
dimension <- getDimension(dataset.all.species, resolution=1)
origin <- getOrigin(dataset.all.species)

##create landwatermask
landwatermask.nocoast <- createLandwatermask(dataset.landwater, 
				dimension, origin, resolution=1)

##estimate species richness
species.richness.weighted <- species.richness(dataset.all.species,
landwatermask.nocoast, distances=1:5, weight=0.5, dimension,
origin, resolution=1, upperbound=3000, all.species=1:2)

##prepare clustersearch
clusterlimit <- 100

##adjust species richness
clusterlist <- searchClusters(species.richness.weighted, 
		dimension, origin, resolution=1, clusterlimit)
}

