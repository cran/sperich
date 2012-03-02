\name{species.richness}
\alias{species.richness}

\title{Species richness estimation}
\description{This function estimates the species richness based on given species occurences.}
\usage{species.richness(dataset.all.species, landwatermask, 
	distances=1:10, weight=0.5, dimension, shift, resolution=1,
	upperbound, narrow.endemic=FALSE, narrow.endemic.limit=5,
	all.species=-1, silent=TRUE)}
\arguments{
  \item{dataset.all.species}{A dataset containing the species with their ID 
			(named: speciesID)and the longitude (named: long) and 
			latitude (named: lat) of their occurence location.}
  \item{landwatermask}{A grid containing the land-water-information of the observed area. 
			If a grid cell containes no land, the value of the cell in the 
			landwatermask is -1, otherwise it is 0. Additional, height-informations could 
		be added for land surfaces. In this case, take care of the 'upperbound' value.}
  \item{distances}{The distances which will be used for species range estimation.}
  \item{weight}{The tuning parameter of the weighting procedure (details in Raedig et al. 2010).}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{upperbound}{This value determines the height (based on values in 'landwatermask') 
		which is considered to be a barrier for species distribution.}
  \item{narrow.endemic}{A boolean flag that determines if only narrow endemic species 
			should be considered in species richness estimation.}
  \item{narrow.endemic.limit}{This value determines the limit of points up to 
			which a species is considered as narrow endemic species.}
  \item{all.species}{The vector with the numbers of the species which should be mentioned. If the first value is -1, 
		all species in the database will be used for species richness estimation.}
  \item{silent}{A boolean flag that determines wether the report of status messages should be suppressed or not. }
}
\details{This routine estimates the species richness based on given species occurences through 
	a geometric interpolation model (details in Raedig et al. 2010).
}
\value{This function returns a grid which contains the weighted 
	species richness information.}
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
		landwatermask.nocoast, distances=1:10, weight=0.5, dimension, 
		shift, resolution=1, upperbound=5, narrow.endemic=FALSE, 
		narrow.endemic.limit=5, all.species=1:20)}
}

