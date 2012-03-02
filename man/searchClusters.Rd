\name{searchClusters}
\alias{searchClusters}

\title{Search clusters in the result grid of species richness estimation.}
\description{This routine searches clusters in the result grid of species richness estimation.}
\usage{searchClusters(species.richness, dimension, shift, resolution, clusterlimit)}
\arguments{
  \item{species.richness}{The grid that should be adjusted. It contains the result of species richness estimation or cross-validation.}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
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
\donttest{data(dataset.all.species)}
\donttest{data(dataset.landwater)}

##create grid parameters
\donttest{dimension <- getDimension(dataset.all.species, resolution=1)}
\donttest{shift <- getShift(dataset.all.species)}

##create landwatermask
\donttest{landwatermask.nocoast <- createLandwatermask(dataset.landwater, 
					dimension, shift, resolution=1)}

##estimate species richness
\donttest{species.richness.weighted <- species.richness(dataset.all.species,
landwatermask.nocoast, distances=1:10, weight=0.5, dimension,
shift, resolution=1, upperbound=3000, all.species=1:20)}

##prepare clustersearch
\donttest{clusterlimit <- 100}

##adjust species richness
\donttest{clusterlist <- searchClusters(species.richness.weighted, 
		dimension, shift, resolution=1, clusterlimit)}
}

