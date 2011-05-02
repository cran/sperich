species.richness.cv <- 
function(dataset.all.species, landwatermask, fold=5, loocv.limit=10, 
		distances=1:10, weight=0.5, dimension, shift, resolution=1, 
		upperbound, all.species=-1, silent=TRUE){
	if (all.species[1]==-1){
		number.of.species <- max(dataset.all.species$speciesID)
		all.species <- 1:number.of.species
	}

	#create grids
	species.richness.weighted.one.species <- matrix(0,dimension[1],dimension[2])
	species.richness.weighted.cv <- matrix(0,dimension[1],dimension[2])


	#iterate about species
	for (species in all.species){
		#initialize temporary array
		species.range.all.subs <- array(0, dim=c(length(distances)-1,dimension[1],dimension[2]))

		#extract datasets of one species out of database
		dataset.one.species <- extract.species(dataset.all.species, species)
		number.of.occurences <- dim(dataset.one.species)[1]

		#more than two occurences needed
		if (number.of.occurences > 2){
			subsamples <- generate.subsamples(number.of.occurences, fold, loocv.limit)

			#iterate about distances
			for (distance in distances[2:length(distances)]){
				#iterate about all subsamples
				for (subsample.id in 1:dim(subsamples)[1]){
					subsample <- subsamples[subsample.id,]
					subsample <- subsample[which(subsample != 0)]
					dataset.one.subsample <- dataset.one.species[subsample,]

					#calculate species range
					species.range.sub <- species.range(dataset.one.subsample, distance, dimension, 
								shift, resolution, landwatermask, upperbound, cross.validation=TRUE)
					
					#sum over all subsamples
					species.range.all.subs[(distance-1),,] <- species.range.all.subs[(distance-1),,] + species.range.sub
				}
				
				#divide through number of subsamples
				species.range.sub.tmp <- species.range.all.subs[(distance-1),,] / matrix(dim(subsamples)[1],dimension[1],dimension[2])
				species.range.sub.tmp[which(is.na(species.range.sub.tmp)==TRUE)] <- 0
				species.range.all.subs[(distance-1),,] <- species.range.sub.tmp				

				if (distance==2){
					species.richness.weighted.one.species <- species.range.all.subs[1,,]
				} else {
					species.richness.weighted.one.species <- species.richness.weighted.one.species + 
						(distance^(-weight) * (species.range.all.subs[(distance-1),,] - species.range.all.subs[(distance-2),,]))
				}
			}

			#sum over all species
			species.richness.weighted.cv <- species.richness.weighted.cv + species.richness.weighted.one.species
			if (!silent)
				cat("Species ",species," done!\n", sep="")
		}		
	}

	return(species.richness.weighted.cv)
}