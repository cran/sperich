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
\donttest{data(dataset.all.species)}
\donttest{data(dataset.landwater)}

##initialize variables
\donttest{distance <- 5}

##create grid parameters
\donttest{dimension <- getDimension(dataset.all.species)}
\donttest{shift <- getShift(dataset.all.species)}

##create landwatermask
\donttest{landwatermask.nocoast <- createLandwatermask(dataset.landwater, dimension, shift)}

##extract datasets of one species out of database
\donttest{dataset.one.species <- extract.species(dataset.all.species, 3)}

##create grid
\donttest{grid <- matrix(0,dimension[1],dimension[2])}

##add points
\donttest{grid <- add.Data.to.Grid(dataset.one.species, dimension, shift)}

##points to list
\donttest{points <- which(grid > 0)}
\donttest{points.xy <- list()}
\donttest{for (i in 1:length(points)){
		points.xy[[i]] <- c(ifelse((points[i] \%\% dimension[1]) == 0, dimension[1], 
					points[i] \%\% dimension[1]), 
					ceiling(points[i]/dimension[1]))
	}
}

##Edge-Water-Test
\donttest{check <- edgeNotValid(grid, points.xy[[1]],points.xy[[2]], 
				landwatermask.nocoast, 1000)}
\donttest{print(check)}
}
