\name{generate.subsamples}
\alias{generate.subsamples}

\title{Subsample Creation}
\description{A function that creates an array of subsamples of a dataset.}
\usage{generate.subsamples(number.of.occurrences, fold, loocv.limit)}
\arguments{
  \item{number.of.occurrences}{The number of occurrences of the species.}
  \item{fold}{The number of groups which should be created if the number of occurrences is greater than loocv.limit.}
  \item{loocv.limit}{The limit below which the subsamples are created for a leave-one-out-cross-validation instead of a k-fold-cross-validation.}
}
\details{This routine creates subsamples of species occurrences based on the number of 
	occurrences of the chosen species and the number of groups which should be created.
	If the number of occurrences of the chosen species is below the loocv.limit, 
	the groups contain only one member and the fold value will be ignored. The subsamples 
	can be used for a k-fold or leave-one-out-cross-validation.
}
\value{This function returns a grid which contains the subsamples.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
subsamples <- generate.subsamples(15,3,5)
}

