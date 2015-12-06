library(R.matlab);

sum_square <- function(land_map,x,y){
	land_x <- (x-1)*10+1;
	land_y <- (y-1)*10+1;
	land_geo_value<-(sum(sum(land_map[(land_x:(land_x+9)),(land_y:(land_y+9))]))>=20);
	return(land_geo_value);
}


land_mat <- readMat('./landseamask.mat');
land_map <- matrix(unlist(land_mat[1]),nrow <- 360, ncol <- 720);
land_geo <- matrix(0,36,72);
for (x in c(1:36)){
	for (y in c(1:72)){
		land_geo[x,y] <- sum_square(land_map,x,y);
	}
}
image(c(1:36),c(1:72),land_geo)

