\name{createHeightmask}
\alias{createHeightmask}

\title{Height-Mask Creation}
\description{This function creates a height-mask as a grid based on given dimension and coordinates.}
\usage{createHeightmask(dataset.height, dimension, shift, resolution=1)}
\arguments{
  \item{dataset.height}{A dataframe containing the longitude and lattitude of the grid 
			cell location as 'long' and 'lat' and the height of the cell as 'height'.
			If the value of 'dataset.height' is 'NULL', the returned grid contains 0 as 
			height-information of every cell.}
  \item{dimension}{The dimension of the processed grid.}
  \item{shift}{The geographic coordinates of the origin of the grid.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
}
\details{This routine creates a height-mask as a grid. As Input a 
	dataset with height-information is needed. The grid will 
	be created with a given dimension.
}
\value{This function returns a grid which contains the height information of the observed area.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
\donttest{data(dataset.all.species)}
\donttest{data(dataset.height)}

##create grid parameters
\donttest{dimension <- getDimension(dataset.all.species, resolution=1)}
\donttest{shift <- getShift(dataset.all.species)}

##create height-matrix
\donttest{height.matrix <- createHeightmask(dataset.height, dimension, shift, resolution=1)}
}

