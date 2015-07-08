\name{evaluate}
\alias{evaluate}

\title{Result grid evaluation}
\description{This function evaluates the result grids of species richness estimation.}
\usage{evaluate(result.grid.one, result.grid.two, 
		title.one="Histogramm of species richness", 
		title.two="Histogramm of species richness", xmax=400, ymax=1000,
		directory=getwd(), filename="histogramm.png")}
\arguments{
	\item{result.grid.one}{A result grid of species richness estimation. If the value is 'NULL', the grid is ignored 
			and only the other grid will be included in the resulting png-file.}
	\item{result.grid.two}{A result grid of species richness estimation. If the value is 'NULL', the grid is ignored 
			and only the other grid will be included in the resulting png-file.}
	\item{title.one}{The title for the histogramm of the first grid ('result.grid.one').}
	\item{title.two}{The title for the histogramm of the second grid ('result.grid.two').}
	\item{xmax}{The maximum value of the abscissa respectively the maximum number of species in the grid.}
	\item{ymax}{The maximum value of the ordinate respectively the maximum frequency shown in the histogramm.}
	\item{directory}{The directory the png-file is created in.}
	\item{filename}{The name of the created png-file.}
}
\details{This routine evaluates the result grids of a species richness estimation (details in Raedig et al. 2010).
	It creates one or two histogramms (depending on input) of the species occurrences with additional information about the 
	total number of species of the grid and other statistic scores. 
	It is possible to create on png-file for every result grid or to compare two result-grids in one png-file, 
	depending on how many grids are defined as input.
}
\value{This function creates a png-file with one or two histogramms (depending on input) and additional statistic informations.}
\references{Raedig, C., Dorman, C.F., Hildebrandt, A. and Lautenbach, S. (2010). 
	Reassessing Neotropical angiosperm distribution patterns based on 
	monographic data: a geometric interpolation approach. Biodivers Conserv, 19, 1523-1546.
}
\author{Maximilian Lange, Sven Lautenbach}

\examples{
#load data
data(dataset.all.species)
data(dataset.landwater)

#create grid parameters
dimension <- getDimension(dataset.all.species, resolution=1)
shift <- getShift(dataset.all.species)

#create landwatermask
landwatermask.nocoast <- createLandwatermask(dataset.landwater,
dimension, shift, resolution=1)

#estimate species richness
species.richness.weighted <- species.richness(dataset.all.species,
landwatermask.nocoast, distances=1:5, weight=0.5, dimension,
shift, resolution=1, upperbound=3000, all.species=1:2)

#evaluation
\dontrun{
evaluate(species.richness.weighted, NULL, title.one="Histogramm 1")
}
}

