\name{getNarrowEndemics}
\alias{getNarrowEndemics}

\title{Narrow endemic species extraction}
\description{This function extracts the row number of narrow endemic species in the given dataset.}
\usage{getNarrowEndemics(dataset.all.species, all.species, 
			narrow.endemic.limit, dimension, origin, 
			resolution)}
\arguments{
  \item{dataset.all.species}{A dataset containing all observed species with their ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of their occurrence location.}
  \item{all.species}{A vector with the numbers of the species which should be mentioned. If the first value is -1, 
		all species in the database will be used.}
  \item{narrow.endemic.limit}{This value determines the limit of points and the maximum distance of these points up to 
		which a species is considered as narrow endemic species.}
  \item{dimension}{The dimension of the grid which should be processed.}
  \item{origin}{The geographic coordinates of the origin of the grid which should be processed.}
  \item{resolution}{The resolution of the grid which should be processed in (geographical) degree.}
}
\details{This function extracts the row numbers of narrow endemic species in the given datasets.}
\value{This function returns a vector containing the row numbers of narrow endemic species in the given dataset.}

\author{Maximilian Lange, Claudia Raedig}

\examples{
## load data
data(dataset.all.species)

## create grid parameters
dimension <- getDimension(dataset.all.species, resolution=1)
origin <- getOrigin(dataset.all.species)

## search narrow endemic species in row 100 to 200 in the dataset
narrow.endemic.species <- getNarrowEndemics(dataset.all.species,
				all.species=100:200, narrow.endemic.limit=8,
				dimension, origin, resolution=1)
}

