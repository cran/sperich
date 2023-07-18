\name{species.richness.nonweighted}
\alias{species.richness.nonweighted}

\title{Species richness estimation without weighting}
\description{This function estimates the species richness for a given distance based on given species occurrences without using weighting factor.}
\usage{species.richness.nonweighted(dataset.all.species, landwatermask, 
	distance=10, dimension, origin, resolution=1,
	upperbound, all.species=-1, silent=TRUE,
	do.parallel=FALSE)}
\arguments{
  \item{dataset.all.species}{A dataset containing the species with their ID 
			(named: speciesID)and the longitude (named: long) and 
			latitude (named: lat) of their occurrence location.}
  \item{landwatermask}{A grid containing the land-water-information of the observed area. 
			If a grid cell containes no land, the value of the cell in the 
			landwatermask is -1, otherwise it is 0. Additional, height-informations could 
		be added for land surfaces. In this case, take care of the 'upperbound' value.}
  \item{distance}{The distance which will be used for species range estimation.}
  \item{dimension}{The dimension of the processed grid.}
  \item{origin}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{upperbound}{This value determines the height (based on values in 'landwatermask') 
		which is considered to be a barrier for species distribution.}
  \item{all.species}{The vector with the numbers of the species which should be mentioned. If the first value is -1, 
		all species in the database will be used for species richness estimation.}
  \item{silent}{A boolean flag that determines wether the report of status messages should be suppressed or not. }
  \item{do.parallel}{A boolean flag determining wether the function uses the 'foreach'-package to estimate species richness via parallel processing.}
}
\details{This routine estimates the species richness for a given distance (without weighted summation) based on given species occurrences through 
	a geometric interpolation model (details in Raedig et al. 2010).
}
\value{This function returns a grid which contains the species richness information for a given distance.}
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
species.richness.noweight <- species.richness.nonweighted(dataset.all.species, 
		landwatermask.nocoast, distance=5, dimension, 
		origin, resolution=1, upperbound=5, all.species=1:2)
}

