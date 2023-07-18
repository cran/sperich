\name{createLandwatermask}
\alias{createLandwatermask}

\title{Land-Water-Mask Creation}
\description{This function creates a land-water-mask as a grid based on given dimension and coordinates.}
\usage{createLandwatermask(dataset.landwater, dimension, origin, resolution=1)}
\arguments{
  \item{dataset.landwater}{A dataset containing the percentage of land on 
			a cell of a grid. The coordinates of the cell should be given as 
			longitude (named: long) and lattude (named: lat) and the percentage 
			of land shoud be named landsum. If the value of 'dataset.landwater' 
			is 'NULL', a grid containing only land-cells will be created.}
  \item{dimension}{The dimension of the processed grid.}
  \item{origin}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
}
\details{This routine creates a land-water-mask as a grid. As Input a 
	dataset with land-water-information is needed. The grid will 
	be created with a given dimension. }

\value{This function returns a grid which contains the land-water information of the observed area. 
		The value of cells containing land is 0, the value of water-cells is -1.}

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
}

