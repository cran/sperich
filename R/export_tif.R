export_tif <- 
function(grid, origin, resolution, epsg_code=4326, directory=getwd(), filename="grid.tif"){
	#create dataframe
	values <- lat <- long <- vector(mode="numeric", length=dim(grid)[1]*dim(grid)[2])
	count <- 1
	for (m in 1:dim(grid)[1]){
		for ( n in 1:dim(grid)[2]){
			values[count] <- grid[m,n]
			long[count] <- origin[1] + resolution*(m-1)
			lat[count] <- origin[2] + resolution*(n-1)
			count <- count + 1
		}
	}
	result <- data.frame(values=values)
	points <- sp::SpatialPoints(data.frame(long=long, lat=lat), proj4string=sp::CRS(paste("+init=epsg:",epsg_code,sep="")))
	#create SpatialGridDataFrame 
	spdf <- sp::SpatialPixelsDataFrame(points, result)	
	sgdf <- as(spdf, "SpatialGridDataFrame")	

	raster::writeRaster(x=raster::raster(sgdf), filename=file.path(directory, filename))
}

