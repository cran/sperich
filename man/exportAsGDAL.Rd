\name{exportAsGDAL}
\alias{exportAsGDAL}

\title{Export result grid as GDAL grid map}
\description{This function exports the result grid as a GDAL grid map.}
\usage{exportAsGDAL(grid, shift, resolution, directory=getwd(), filename="grid.tif", drivername="GTiff")}
\arguments{
  \item{grid}{The grid that should be exported.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{directory}{The directory in which the file will be created.}
  \item{filename}{The name of the created file containing the GDAL grid map.}
  \item{drivername}{Determines the format of the resulting GDAL grid map. 
		All available drivers can be shown by using the command 'gdalDrivers()'.}
}
\details{This routine exports a grid as an GDAL grip map. The package rgdal is required.}

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

##estimate species richness
species.richness.weighted <- species.richness(dataset.all.species,
	landwatermask.nocoast, distances=1:5, weight=0.5, dimension,
	shift, resolution=1, upperbound=3000, all.species=1:2)

##export
exportAsGDAL(species.richness.weighted, shift, resolution=1, 
	    directory=getwd(), filename="species.richness.tif", drivername="GTiff")
}

