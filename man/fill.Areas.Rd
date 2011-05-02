\name{fill.Areas}
\alias{fill.Areas}

\title{Fill Algorithm}
\description{This function fills all bounded areas in a grid.}
\usage{fill.Areas(grid, landwatermask)}
\arguments{
  \item{grid}{A grid in which the areas should be filled. Cells with values greater than zero 
		represent the bounds, cells with values lower or equal zero will be filled with 
		value 1 if they are bounded.}
  \item{landwatermask}{A grid containing the land-water-information of the observed area. 
		If a grid cell containes no land, the value of the cell in the 
		landwatermask is -1, otherwise it is 0. Cells with negative values will not be filled.}

}
\details{This function fills all bounded areas in a grid.}
\value{The grid with filled areas.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##initialize data
\donttest{m <- matrix(0,10,10)}
\donttest{m[2:8,2] <- 1}
\donttest{m[2:8,8] <- 1}
\donttest{m[2,2:8] <- 1}
\donttest{m[8,2:8] <- 1}

##initialize mask (in this case: empty mask)
\donttest{mask <- matrix(0,10,10)}

##calculate the distance of the two points
\donttest{m <- fill.Areas(m, mask)}
\donttest{image(m)}
}

