\name{createImage}
\alias{createImage}

\title{Image Creation}
\description{This function creates a levelplot of the result-grid of species range estimation and saves it as a PNG-File.}
\usage{createImage(grid, landwatermask, image.title, 
			directory, filename, shift, 
			parts=10, resolution=1)}
\arguments{
  \item{grid}{The grid that should be plotted. It contains the weighted species richness information.}
  \item{landwatermask}{A grid containing the land-water-information of the observed area. 
			If a grid cell containes no land, the value of the cell in the 
			landwatermask is -1, otherwise it is 0.}
  \item{image.title}{The heading of the created image.}
  \item{directory}{The directory in which the image should be stored.}
  \item{filename}{The filename of the created PNG-File.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{parts}{An Integer value determining the number of divisions in the colorkey of the created image.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
}
\details{This routine creates a levelplot of the weighted 
	species richness information and stores it as a PNG-File.
	The package lattice is required.}
\value{The return type is boolean. The value is TRUE if the image-creation 
	was successful and FALSE if the image-creation has failed.}

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

##create image
image.success <- createImage(species.richness.weighted, landwatermask.nocoast, 
	    image.title="Test", directory=getwd(), filename="species.richness.test01.png", 
	    shift, parts=10, resolution=1)
print(image.success)
}

