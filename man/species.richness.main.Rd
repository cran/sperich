\name{species.richness.main}
\alias{species.richness.main}

\title{Main Function for species richness estimation}
\description{The main function to estimate or cross-validate the species richness based on given species occurences.}
\usage{species.richness.main(dataset.all.species, dataset.landwater, 
	dataset.height, distance.max=10, weight=0.5, resolution=1,
	narrow.endemic=FALSE, narrow.endemic.limit=5, upperbound=5, 
	cross.validation=FALSE, fold=5, loocv.limit=10, 
	create.image=FALSE, image.title="Interpolated Species Richness", 
	directory=getwd(), filename="species.richness.png", 
	evaluation=FALSE, eval.title="Histogramm", adjust=FALSE, 
	clusterlimit=100, predefinedClusterlist=NULL, all.species=-1, 
	export=FALSE, drivername="GTiff", exportname="species.richness.tif", 
	noninterpolatedgrid=NULL, silent=TRUE)}
\arguments{
  \item{dataset.all.species}{A dataset containing all observed species with their ID 
			(named: speciesID) and the longitude (named: long) and 
			latitude (named: lat) of their occurence location.}
  \item{dataset.landwater}{A dataset containing the percentage of land on 
			a cell of a grid. The coordinates
			of the cell should be given as longitude (named: long) and 
			lattude (named: lat) and the percentage of land shoud be named landsum. 
			If the value of 'dataset.landwater' is 'NULL', the land-water-information 
			has no effect on the species richness estimation.}
  \item{dataset.height}{A dataset containing the longitude and lattitude of the grid cell location 
			as 'long' and 'lat' and the height of the cell as 'height'. If the value of 
			'dataset.height' is 'NULL', the height-information has no effect on the 
			species richness estimation.}
  \item{distance.max}{The maximum distance is used to generate a vector of distances (1:distance.max) 
				which will be used for species range estimation.}
  \item{weight}{The tuning parameter of the weighting procedure (details in Raedig et al. 2010).}
  \item{resolution}{The resolution of the grid in (geographical) degree.}
  \item{narrow.endemic}{A boolean flag that determines if only narrow endemic species 
			should be considered in species richness estimation.}
  \item{narrow.endemic.limit}{This value determines the limit of points up to 
			which a species is considered as narrow endemic species.}
  \item{upperbound}{This value determines the height which is considered to be a barrier for species distribution.}
  \item{cross.validation}{A logical value determining wether a cross-validation is performed. If the value is true, 
				the parameters narrow.endemic and narrow.endemic.limit will be ignored.}
  \item{fold}{The number of groups which should be created if the number of occurences is greater than loocv.limit.}
  \item{loocv.limit}{The limit below which the subsamples are created for a leave-one-out-cross-validation instead of a k-fold-cross-validation.}
  \item{create.image}{A boolean flag that determines if an image (PNG-File) should be created.}
  \item{image.title}{The heading of the created image.}
  \item{directory}{The directory in which the created files should be stored.}
  \item{filename}{The filename of the created PNG-Files.}
  \item{evaluation}{A boolean value determining wether the routine 'evaluate' is used or not. If the value is true, a PNG-File with a histogramm of the result grid will be created.}
  \item{eval.title}{The heading of the created histogramm.}
  \item{adjust}{A boolean value determining wether an adjustment of the result grid should be done or not.}
  \item{clusterlimit}{The limit under which values of 'species.richness' should not be used to build clusters. 
			Each cluster will be adjusted with an own centre of species richness.}
  \item{predefinedClusterlist}{A list of vectors of pixelpositions (created by 'searchClusters') which are spatial related. 
					The default value is NULL because the list will be created if 'adjust' is 'TRUE', 
					but it may be useful for robustness estimation, because the cross-validation result should be 
					adjusted with the same clusterlist as the related species richness estimation 
					(and without a pre-defined clusterlist, other clusters will be build).}
  \item{all.species}{The vector with the numbers of the species which should be mentioned. If the first value is -1, 
		all species in the database will be used for species richness estimation.}
  \item{export}{A boolean value that determines if the routine should export the results as GDAL grid map.}
  \item{drivername}{Determines the format of the resulting GDAL grid map. 
		All available drivers can be shown by using the command 'gdalDrivers()'.}
  \item{exportname}{The name of the created file containing the GDAL grid map.}
  \item{noninterpolatedgrid}{A grid containing the species occurences (could be created via function 'createNonInterpolatedGrid').
		If the value of this parameter is 'NULL', it will be created if an adjustment should be done. 
		The parameter may be usefull to save time while processing more than one 'species.richness.main'.}
  \item{silent}{A boolean flag that determines wether the report of status messages should be suppressed or not. }
}
\details{This routine is the main function of this package. It eithor estimates or 
	cross-validates the species richness based on given species occurences through 
	a geometric interpolation model (details in Raedig et al. 2010).
}
\value{This function returns a grid which contains either the weighted or the 
	cross-validated species richness information. Additionally, an 
	image (PNG-File) of that grid could be created.
}
\references{Raedig, C., Dorman, C.F., Hildebrandt, A. and Lautenbach, S. (2010). 
	Reassessing Neotropical angiosperm distribution patterns based on 
	monographic data: a geometric interpolation approach. Biodivers Conserv, 19, 1523-1546.
}
\author{Maximilian Lange, Sven Lautenbach}

\examples{
##load data
\donttest{data(dataset.all.species)}
\donttest{data(dataset.landwater)}
\donttest{data(dataset.height)}

##estimate species richness
\donttest{species.richness.weighted <- species.richness.main(dataset.all.species, 	
	    dataset.landwater, dataset.height, distance.max=10, weight=0.5, 
	    resolution=1, narrow.endemic=FALSE, narrow.endemic.limit=5, 
	    upperbound=5, cross.validation=FALSE, fold=5, loocv.limit=10, 
	    create.image=FALSE, directory=getwd(), filename="species.richness.png", 
	    all.species=1:20, export=FALSE)}
}

