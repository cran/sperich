species.richness.nonweighted <- 
function(dataset.all.species, landwatermask, distance=10, 
		dimension, shift, resolution=1, upperbound, 
		narrow.endemic=FALSE, narrow.endemic.limit=5, 
		all.species=-1, silent=TRUE){
	if (all.species[1]==-1){
		all.species <- unique(dataset.all.species$speciesID)
	} else {
		all.species.tmp <- c()
		for (species in all.species){
			if (length(which(dataset.all.species$speciesID == species)==TRUE) > 0){
				all.species.tmp <- c(all.species.tmp, species)
			}
		}
		all.species <- all.species.tmp
	}
	number.of.species <- length(all.species)
	message <- ""

	#create grid
	species.richness.noweight <- matrix(0, dimension[1], dimension[2])

	for (species in all.species){
		dataset.one.species <- extract.species(dataset.all.species, species)
		species.range.d <- species.range(dataset.one.species, distance, 
						dimension, shift, resolution, landwatermask, 
						upperbound)
		if (narrow.endemic){
			#sum over all species with maximum interpolated range size of 'narrow.endemic.limit'
			if (length(which(species.range.d > 0)) <= narrow.endemic.limit){
				species.richness.noweight <- species.richness.noweight + species.range.d
			}
		} else {
			#sum over all species
			species.richness.noweight <- species.richness.noweight + species.range.d
		}
		
		if (!silent){
			cat(rep("\b", nchar(message)),sep="")
			message <- paste("Species ",which(species==all.species)," of ",number.of.species," done!", sep="")
			cat(message)
			flush.console()
		}	
	}
	
	if (!silent){
		cat("\n")
	}

	return(species.richness.noweight)
}
