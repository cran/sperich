species.richness <- 
function(dataset.all.species, landwatermask, distances=1:10, weight=0.5, 
		dimension, shift, resolution=1, upperbound, 
		narrow.endemic=FALSE, narrow.endemic.limit=5, 
		all.species=-1, silent=TRUE){
	#check species
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

	#create grids
	species.richness.weighted <- matrix(0, dimension[1], dimension[2])
	species.richness.weighted.one.species <- matrix(0, dimension[1], dimension[2])
	species.range.distance <- array(0, dim=c(length(distances),dimension[1], dimension[2]))

	for (species in all.species){
		dataset.one.species <- extract.species(dataset.all.species, species)
		for (distance in distances){
			species.range.distance[distance,,] <- species.range(dataset.one.species, distance, 
										dimension, shift, resolution, landwatermask, 
										upperbound)
			if (distance==1){
				species.richness.weighted.one.species <- species.range.distance[1,,]
			} else {
				species.richness.weighted.one.species <- species.richness.weighted.one.species + 
				(distance^(-weight) * (species.range.distance[distance,,] - species.range.distance[distance-1,,]))
			}
		}
		if (narrow.endemic){
			#sum over all species with maximum interpolated range size of 'narrow.endemic.limit'
			if (length(which(species.richness.weighted.one.species > 0)) <= narrow.endemic.limit){
				species.richness.weighted <- species.richness.weighted + species.richness.weighted.one.species
			}
		} else {
			#sum over all species
			species.richness.weighted <- species.richness.weighted + species.richness.weighted.one.species
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

	return(species.richness.weighted)
}
