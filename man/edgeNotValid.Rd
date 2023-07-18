\name{edgeNotValid}
\alias{edgeNotValid}

\title{Edge-Water-Test}
\description{This function checks if an edge would cross a water or mountain surface.}
\usage{edgeNotValid(grid,point.a, point.b, landwatermask, upperbound)}
\arguments{
  \item{grid}{A grid to which the edge should be added if it hits no water or mountain surfaces.}
  \item{point.a}{A point in a grid given by its row and column.}
  \item{point.b}{A point in a grid given by its row and column.}
  \item{landwatermask}{A grid containing the land-water-information of the observed area. 
		If a grid cell containes no land, the value of the cell in the 
		landwatermask is -1, otherwise it is 0. Additional, height-informations could 
		be added for land surfaces. In this case, take care of the 'upperbound' value.}
  \item{upperbound}{This value determines the height (based on values in 'landwatermask') 
		which is considered to be a barrier for species distribution.}
}
\details{This function checks if an edge would cross a water or mountain surface.}
\value{A boolean value which determines if the edge would cross a water or mountain surface.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
data(dataset.all.species)
data(dataset.landwater)

##initialize variables
distance <- 5

##create grid parameters
dimension <- getDimension(dataset.all.species)
origin <- getOrigin(dataset.all.species)

##create landwatermask
landwatermask.nocoast <- createLandwatermask(dataset.landwater, dimension, origin)

##extract datasets of one species out of database
dataset.one.species <- extract.species(dataset.all.species, 3)

##create grid
grid <- matrix(0,dimension[1],dimension[2])

##add points
grid <- data.into.Grid(dataset.one.species, dimension, origin)

##points to list
points <- which(grid > 0)
points.xy <- list()
for (i in 1:length(points)){
	points.xy[[i]] <- c(ifelse((points[i] \%\% dimension[1]) == 0, dimension[1], 
			points[i] \%\% dimension[1]), ceiling(points[i]/dimension[1]))
}

##Edge-Water-Test
check <- edgeNotValid(grid, points.xy[[1]],points.xy[[2]], 
			landwatermask.nocoast, 1000)
print(check)
}
