\name{cclFun}
\alias{cclFun}

\title{Connected Component Labeling}
\description{This routine identifies disjunct patches within a distribution matrix.}
\usage{cclFun(mat)}
\arguments{
  \item{mat}{Matrix with 0 as background value and positive values as occurrences to check patches for.}
}
\details{This routine identifies disjunct patches of positive values within a matrix containing zeros as background values.}
\value{This function returns a matrix with zero as background value and positive values numbering and representing the disjunct patches.}
\author{Maximilian Lange}

\examples{
## example data
tmat = { matrix(c( 0,0,0,1,0,0,1,1,0,1,
                   0,0,1,0,1,0,0,0,0,0,
                   0,1,NA,1,0,1,0,0,0,1,
                   1,0,1,1,1,0,1,0,0,1,
                   0,1,0,1,0,1,0,0,0,1,
                   0,0,1,0,1,0,0,1,1,0,
                   1,0,0,1,0,0,1,0,0,1,
                   0,1,0,0,0,1,0,0,0,1,
                   0,0,1,1,1,0,0,0,0,1,
                   1,1,1,0,0,0,0,0,0,1),
		nr=10,byrow=TRUE) }

ccl.mat <- cclFun(tmat)
}

