\name{getDistance}
\alias{getDistance}

\title{Distance between two points in a grid}
\description{This function calculates the distance of two points in a grid.}
\usage{getDistance(point.a, point.b, resolution=1)}
\arguments{
  \item{point.a}{A point in a grid given by its row and column.}
  \item{point.b}{A point in a grid given by its row and column.}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
}
\details{This function calculates the distance of two points in a grid by the Pythagorean theorem.}
\value{The distance of the two points as an numeric value based on geographical degrees.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##initialize data
point.a <- c(5,8)
point.b <- c(10,3)

##calculate the distance of the two points
distance <- getDistance(point.a,point.b, resolution=1)
print(distance)
}

