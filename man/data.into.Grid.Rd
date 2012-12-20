\name{data.into.Grid}
\alias{data.into.Grid}

\title{Species occurrences to grid}
\description{This function adds the species occurrence data to a new grid with given dimension, shift and resolution.}
\usage{data.into.Grid(dataset.one.species, dimension, shift, resolution=1)}
\arguments{
  \item{dataset.one.species}{A dataset containing one species with its ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of the occurrence locations of that species.}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
}
\details{This routine adds the species occurrence data to the grid through 
		adding their location as a point into it.
}
\value{This function returns a grid which contains the species occurrence information.}
\references{Raedig, C., Dorman, C.F., Hildebrandt, A. and Lautenbach, S. (2010). 
	Reassessing Neotropical angiosperm distribution patterns based on 
	monographic data: a geometric interpolation approach. Biodivers Conserv, 19, 1523-1546.
}
\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
data(dataset.all.species)

##create grid parameters
dimension <- getDimension(dataset.all.species, resolution=1)
shift <- getShift(dataset.all.species)

#extract datasets of one species out of database
dataset.one.species <- extract.species(dataset.all.species, 3)

##add species occurrence information to grid
grid <- data.into.Grid(dataset.one.species, dimension, shift, resolution=1)
}

