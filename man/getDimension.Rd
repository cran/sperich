\name{getDimension}
\alias{getDimension}

\title{getDimension}
\description{This function calculates the dimension of the grid which should be used for a species richness estimation.}
\usage{getDimension(dataset.all.species, resolution=1)}
\arguments{
  \item{dataset.all.species}{A dataset containing all observed species with their ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of their occurrence location.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
}
\details{This function calculates the dimension of the grid which 	
	should be used for a species richness estimation on a given 
	database of species occurrence locations.
}
\value{The dimension which is needed to create a grid used 
	for a species richness estimation on a given database 
	of species occurrence locations.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
data(dataset.all.species)

#extract dimension out of the database
dimension <- getDimension(dataset.all.species, resolution=1)
print(dimension)
}

