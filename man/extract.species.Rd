\name{extract.species}
\alias{extract.species}

\title{Species Extraktion}
\description{This function extracts the datasets of one species out of a database filled with species and their occurence locations.}
\usage{extract.species(dataset.all.species, species.number)}
\arguments{
  \item{dataset.all.species}{A dataset containing all observed species with their ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of their occurence location.}
  \item{species.number}{The number of the species which should be extracted.}
}
\details{This function extracts the datasets of one species out of a database 
		filled with species (named by their speciesID) and their occurence 
		locations mentioned with lattitude (named lat) and longitude (named long).
}
\value{This function returns a dataset which contains the species occurence locations.}

\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
\donttest{data(dataset.all.species)}

#extract datasets of one species (with speciesID=3) out of database
\donttest{dataset.one.species <- extract.species(dataset.all.species, 3)}
}

