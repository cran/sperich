\name{adjustment}
\alias{adjustment}

\title{Adjustment of interpolated species richness}
\description{This routine adjusts the weighted species richness for sampling effort.}
\usage{adjustment(species.richness, noninterpolatedgrid, clusterlist)}
\arguments{
  \item{species.richness}{The grid that should be adjusted. It contains the result of species richness estimation or cross-validation.}
  \item{noninterpolatedgrid}{A grid containing the species occurences (could be created via function 'createNonInterpolatedGrid').}
  \item{clusterlist}{A list of vectors of pixelpositions (created by 'searchClusters') which are spatial related.}
}
\details{This routine adjusts the interpolated species richness for sampling effort 
	by incorporating an additional weighting factor. Therefor, it needs clusters 
	of pixels of the result grid to find centers of species richness and to create these weighting factor. }
\value{This function returns a grid containing the adjusted species richness information.}
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
		landwatermask.nocoast, distances=1:10, weight=0.5, dimension, shift, resolution=1,
		upperbound=5, narrow.endemic=FALSE, narrow.endemic.limit=5, 
		all.species=1:20)}

##prepare adjustment
\donttest{noninterpolatedgrid <- createNonInterpolatedGrid(dataset.all.species, 
				dimension, shift, resolution=1, all.species=-1)}
\donttest{clusterlimit <- 100}

\donttest{clusterlist <- searchClusters(species.richness.weighted, 
		dimension, shift, resolution=1, clusterlimit)}

##adjust species richness
\donttest{species.richness.weighted.adjusted <- adjustment(species.richness.weighted, 
		noninterpolatedgrid, clusterlist)}
}

