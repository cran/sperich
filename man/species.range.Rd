\name{species.range}
\alias{species.range}

\title{Species range estimation}
\description{This function estimates the species range based on given occurrences of one species.}
\usage{species.range(dataset.one.species, distance, dimension, shift, 
	resolution=1, landwatermask, upperbound, cross.validation=FALSE)}
\arguments{
  \item{dataset.one.species}{A dataset containing one species with its ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of the occurrence locations of that species.}
  \item{landwatermask}{A grid containing the land-water-information of the observed area. 
			If a grid cell containes no land, the value of the cell in the 
			landwatermask is -1, otherwise it is 0. Additional, height-informations could 
			be added for land surfaces. In this case, take care of the 'upperbound' value.}
  \item{distance}{The maximum distance in which two occurrences are considered as related occurrences. 
		These two occurrences will be connected with an edge.}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{upperbound}{This value determines the height (based on values in 'landwatermask') 
		which is considered to be a barrier for species distribution.}
  \item{cross.validation}{A logical value determining wether a cross-validation is performed.}
}
\details{This routine estimates the species range based on given occurrences of one species through 
	a geometric interpolation model (details in Raedig et al. 2010).
}
\value{This function returns a grid which contains the species range information.}
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
shift <- getShift(dataset.all.species)

##create landwatermask
landwatermask.nocoast <- createLandwatermask(dataset.landwater, 
					dimension, shift, resolution=1)

##extract datasets of one species out of database
dataset.one.species <- extract.species(dataset.all.species, 3)

##estimate species range
species.range.tmp <- species.range(dataset.one.species, distance=5, 
		dimension, shift, resolution=1, landwatermask.nocoast)
}

