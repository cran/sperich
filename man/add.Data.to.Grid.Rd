\name{add.Data.to.Grid}
\alias{add.Data.to.Grid}

\title{Add Data to Grid}
\description{This function adds the species occurence data to the grid.}
\usage{add.Data.to.Grid(dataset.one.species, dimension, shift, resolution=1)}
\arguments{
  \item{dataset.one.species}{A dataset containing one species with its ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of the occurence locations of that species.}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
}
\details{This routine adds the species occurence data to the grid through 
		adding their location as a point into it.
}
\value{This function returns a grid which contains the species occurence information.}
\references{Raedig, C., Dorman, C.F., Hildebrandt, A. and Lautenbach, S. (2010). 
	Reassessing Neotropical angiosperm distribution patterns based on 
	monographic data: a geometric interpolation approach. Biodivers Conserv, 19, 1523-1546.
}
\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
\donttest{data(dataset.all.species)}

##create grid parameters
\donttest{dimension <- getDimension(dataset.all.species, resolution=1)}
\donttest{shift <- getShift(dataset.all.species)}

#extract datasets of one species out of database
\donttest{dataset.one.species <- extract.species(dataset.all.species, 3)}

##add species occurence information to grid
\donttest{grid <- add.Data.to.Grid(dataset.one.species, dimension, shift, resolution=1)}
}

