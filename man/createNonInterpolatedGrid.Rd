\name{createNonInterpolatedGrid}
\alias{createNonInterpolatedGrid}

\title{Create a Species Occurences Grid with no interpolation}
\description{This routine creates a grid containing the species occurences.}
\usage{createNonInterpolatedGrid(dataset.all.species, dimension, 
			shift, resolution=1, all.species=-1)}
\arguments{
  \item{dataset.all.species}{A dataset containing all observed species with their ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of their occurence location.}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{all.species}{The vector with the numbers of the species which should be mentioned. If the first value is -1, 
		all species in the database will be added to the grid.}
}

\details{This routine creates a grid and adds all species mentioned in 'all.species' to it.}

\value{A grid containing the species occurences without interpolation.}

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
\donttest{resolution <- 1}
\donttest{all.species <- -1}

##estimate species richness
\donttest{species.occurences <- createNonInterpolatedGrid(dataset.all.species, 
				dimension, shift, resolution, all.species)}
}

