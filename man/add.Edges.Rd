\name{add.Edges}
\alias{add.Edges}

\title{Edge Creation}
\description{This function adds an edge between two points to a grid.}
\usage{add.Edges(grid,point.a, point.b)}
\arguments{
  \item{grid}{A grid to which the edge should be added.}
  \item{point.a}{A point in a grid given by its row and column.}
  \item{point.b}{A point in a grid given by its row and column.}
}
\details{This function adds an edge between two points to a grid.}
\value{The grid with new edge included.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##initialize data
\donttest{m <- matrix(0,10,10}
\donttest{point.a <- c(5,8)}
\donttest{point.b <- c(10,3)}

##calculate the distance of the two points
\donttest{m <- add.Edge(m,point.a,point.b)}
\donttest{image(m)}
}

