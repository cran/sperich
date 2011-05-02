createNonInterpolatedGrid <-
function(dataset.all.species, dimension, shift, resolution=1, all.species=-1){
	if (all.species[1]==-1){
		number.of.species <- max(dataset.all.species$speciesID)
		all.species <- 1:number.of.species
	}
	
	#create grid
	grid <- matrix(0,dimension[1],dimension[2])

	for (species in all.species){
		dataset.one.species <- extract.species(dataset.all.species, species)
		grid <- grid + add.Data.to.Grid(dataset.one.species, dimension, shift, resolution)
	}

	return(grid)
}