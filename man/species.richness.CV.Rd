\name{species.richness.cv}
\alias{species.richness.cv}

\title{Species richness cross-validation}
\description{This function cross-validates the species richness based on given species occurrences.}
\usage{species.richness.cv(dataset.all.species, landwatermask, fold=5, 
	loocv.limit=10, distances=3:10, weight=0.5, dimension, shift, 
	resolution=1, upperbound, all.species=-1, 
	silent=TRUE, do.parallel=FALSE)}
\arguments{
  \item{dataset.all.species}{A dataset containing the species with their ID 
			(named: speciesID)and the longitude (named: long) and 
			latitude (named: lat) of their occurrence location.}
  \item{landwatermask}{A grid containing the land-water-information of the observed area. 
			If a grid cell containes no land, the value of the cell in the 
			landwatermask is -1, otherwise it is 0. Additional, height-informations could 
		be added for land surfaces. In this case, take care of the 'upperbound' value.}
  \item{fold}{The number of groups which should be created if the number of occurrences is greater than loocv.limit.}
  \item{loocv.limit}{The limit below which the subsamples are created for a leave-one-out-cross-validation instead of a k-fold-cross-validation.}
  \item{distances}{The distances which will be used for species range estimation.}
  \item{weight}{The tuning parameter of the weighting procedure (details in Raedig et al. 2010).}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{upperbound}{This value determines the height (based on values in 'landwatermask') 
		which is considered to be a barrier for species distribution.}
  \item{all.species}{The vector with the numbers of the species which should be mentioned. If the first value is -1, 
		all species in the database will be used for species richness estimation.}
  \item{silent}{A boolean flag that determines wether the report of status messages should be suppressed or not. }
  \item{do.parallel}{A boolean flag determining wether the function uses the 'foreach'-package to cross-validate via parallel processing.}
}
\details{This routine cross-validates the species richness based on given species occurrences through 
	a geometric interpolation model (details in Raedig et al. 2010).
}
\value{This function returns a grid which contains the cross-validated 
	species richness information.}
\references{Raedig, C., Dorman, C.F., Hildebrandt, A. and Lautenbach, S. (2010). 
	Reassessing Neotropical angiosperm distribution patterns based on 
	monographic data: a geometric interpolation approach. Biodivers Conserv, 19, 1523-1546.
}
\author{Maximilian Lange, Sven Lautenbach, Claudia Raedig}

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

##estimate species richness
species.richness.weighted <- species.richness.cv(dataset.all.species, 
		landwatermask.nocoast, fold=5, loocv.limit=10, distances=2:5, 
		weight=0.5, dimension, shift, resolution=1, upperbound=5, 
		all.species=1:2)
}

