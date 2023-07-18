\name{export_tif}
\alias{export_tif}

\title{Export result grid as GeoTiff}
\description{This function exports the result grid as a GeoTiff.}
\usage{export_tif(grid, origin, resolution, epsg_code, directory=getwd(), 
		filename="grid.tif")}
\arguments{
  \item{grid}{The grid that should be exported.}
  \item{origin}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{epsg_code}{The epsg_code of the origin coordines. Defauls to 4326.}
  \item{directory}{The directory in which the file will be created.}
  \item{filename}{The name of the created GeoTiff-file.}
}
\details{This routine exports a grid as a GeoTiff-file.}

\author{Maximilian Lange}

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
species.richness.weighted <- species.richness(dataset.all.species,
	landwatermask.nocoast, distances=1:5, weight=0.5, dimension,
	origin, resolution=1, upperbound=3000, all.species=1:2)

##export
\dontrun{ 
export_tif(species.richness.weighted, origin, resolution=1, epsg_code="4326",
	    directory=getwd(), filename="species.richness.tif")
}
}

