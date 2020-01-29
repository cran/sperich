cclFun <- function(mat){
	stepIn <- function(mat,cluster,x,y,clusterNr){
		if ((x<=0) || (y <= 0)) return(cluster)
		if (x > dim(mat)[1]) return(cluster)
		if (y > dim(mat)[2]) return(cluster)
		if (!is.na(cluster[x,y])) return(cluster)
		if (is.na(mat[x,y])) return(cluster)
		if (!is.na(cluster[x,y])) return(cluster)

		if (mat[x,y]>0){
			cluster[x,y] <- clusterNr
			
			cluster <- stepIn(mat,cluster,x-1,y+1,clusterNr)
			cluster <- stepIn(mat,cluster,x,y+1,clusterNr)
			cluster <- stepIn(mat,cluster,x+1,y+1,clusterNr)

			cluster <- stepIn(mat,cluster,x-1,y,clusterNr)
			cluster <- stepIn(mat,cluster,x+1,y,clusterNr)

			cluster <- stepIn(mat,cluster,x-1,y-1,clusterNr)
			cluster <- stepIn(mat,cluster,x,y-1,clusterNr)
			cluster <- stepIn(mat,cluster,x+1,y-1,clusterNr)
		}
		return(cluster)
	}
	cluster <- matrix(NA, dim(mat)[1], dim(mat)[2])

	clusterNr <- 1 
	for (x in 1:dim(mat)[1]){
		for (y in 1:dim(mat)[2]){
			if (is.na(mat[x,y])) { cluster[x,y] <- 0; next; }
			if (mat[x,y]==0) cluster[x,y] <- 0;
			if (!is.na(cluster[x,y])) next;
	
			cluster <- stepIn(mat,cluster,x,y,clusterNr)
			clusterNr <- clusterNr+1;
		}
	}

	cluster[which(is.na(mat))] <- NA
	return(cluster)
}

