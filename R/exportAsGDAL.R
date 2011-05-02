exportAsGDAL <- 
function(grid, shift, resolution, directory=getwd(), filename="grid.tif", drivername="GTiff"){
	require(rgdal)

	#check directory
	direc <- unlist(strsplit(directory,""))
	if (direc[length(direc)] != "/"){
		direc <- c(direc, "/")
	}
	directory <- ""
	for (i in 1:length(direc)){
		directory <- paste(directory,direc[i], sep="")
	}
	#check filename
	file <- unlist(strsplit(filename,""))
	filename <- ""
	if ((file[length(file)-3]!=".")||(file[length(file)-2]!="p")||(file[length(file)-1]!="n")||(file[length(file)]!="g")){
		file <- c(file,".","p","n","g")
	}
	for (i in 1:length(file)){
			filename <- paste(filename,file[i], sep="")
	}

	#get Dimension
	dimension <- dim(grid)

	#create dataframe
	values <- lat <- long <- vector(mode="numeric", length=dimension[1]*dimension[2])
	count <- 1
	for (m in 1:dimension[1]){
		for ( n in 1:dimension[2]){
			values[count] <- grid[m,n]
			long[count] <- shift[1] + resolution*n
			lat[count] <- shift[2] + resolution*m
			count <- count + 1
		}
	}
	result <- data.frame(values=values)
	coordinates(result) <- data.frame(long=long, lat=lat)

	result.spatial <- as(result, "SpatialPixelsDataFrame")	
	result.gdal <- as(result.spatial, "SpatialGridDataFrame")	

	writeGDAL(result.gdal, fname=paste(directory,filename, sep=""), drivername=drivername)
}