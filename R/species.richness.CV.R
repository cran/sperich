species.richness.cv <- 
function(dataset.all.species, landwatermask, fold=5, loocv.limit=10, 
		distances=2:10, weight=0.5, dimension, shift, resolution=1, 
		upperbound, all.species=-1, silent=TRUE){
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

	#check distances
	if (distances[1] != 2){
		cat("Error: First distance has to be 2!\n")
		return(NULL)
	}

	message <- ""

	#create grids
	species.richness.weighted.one.species <- matrix(0,dimension[1],dimension[2])
	species.richness.weighted.cv <- matrix(0,dimension[1],dimension[2])

	#iterate about species
	for (species in all.species){
		#initialize temporary array
		species.range.all.subs <- array(0, dim=c(length(distances),dimension[1],dimension[2]))

		#extract datasets of one species out of database
		dataset.one.species <- extract.species(dataset.all.species, species)
		number.of.occurences <- dim(dataset.one.species)[1]

		#more than two occurences needed
		if (number.of.occurences > 2){
			subsamples <- generate.subsamples(number.of.occurences, fold, loocv.limit)

			#iterate about distances
			for (distance in distances){
				#iterate about all subsamples
				for (subsample.id in 1:dim(subsamples)[1]){
					subsample <- subsamples[subsample.id,]
					subsample <- subsample[which(subsample != 0)]
					dataset.one.subsample <- dataset.one.species[subsample,]

					#calculate species range
					species.range.sub <- species.range(dataset.one.subsample, distance, dimension, 
								shift, resolution, landwatermask, upperbound, cross.validation=TRUE)
					
					#sum over all subsamples
					species.range.all.subs[which(distance == distances),,] <- species.range.all.subs[which(distance == distances),,] + species.range.sub
				}
				
				#divide through number of subsamples
				species.range.sub.tmp <- species.range.all.subs[which(distance == distances),,] / matrix(dim(subsamples)[1],dimension[1],dimension[2])
				species.range.sub.tmp[which(is.na(species.range.sub.tmp)==TRUE)] <- 0
				species.range.all.subs[which(distance == distances),,] <- species.range.sub.tmp				

				if (which(distance==distances)==1){
					species.richness.weighted.one.species <- species.range.all.subs[1,,]
				} else {
					species.richness.weighted.one.species <- species.richness.weighted.one.species + 
						(distance^(-weight) * (species.range.all.subs[which(distance == distances),,] - species.range.all.subs[which(distance == distances)-1,,]))
				}
			}

			#sum over all species
			species.richness.weighted.cv <- species.richness.weighted.cv + species.richness.weighted.one.species
			
			if (!silent){
				cat(rep("\b", nchar(message)),sep="")
				message <- paste("Species ",which(species==all.species)," of ",number.of.species," done!", sep="")
				cat(message)
				flush.console()
			}
		}		
	}

	if (!silent)
		cat("\n")

	return(species.richness.weighted.cv)
}
