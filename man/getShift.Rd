\name{getShift}
\alias{getShift}

\title{Origin Koordinates}
\description{This function calculates the origin of the grid used for a species richness estimation.}
\usage{getShift(dataset.all.species)}
\arguments{
  \item{dataset.all.species}{A dataset containing all observed species with their ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of their occurrence location.}
}
\details{This function calculates the coordinates of the origin of the grid which 
	should be used for a species richness estimation.
}
\value{The coordinates of the origin of the grid used for species richness estimation.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
data(dataset.all.species)

#extract coordinates of origin out of database
shift <- getShift(dataset.all.species)
print(shift)
}

