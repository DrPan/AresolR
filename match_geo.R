library(R.matlab);


sum_square <- function(land_map,x,y){
	land_x <- (x-1)*10+1;
	land_y <- (y-1)*10+1;
	land_geo_value<-(sum(sum(land_map[(land_x:(land_x+9)),
		(land_y:(land_y+9))]))>=30);
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

land_real=matrix(NA, nrow=72, ncol=36)

for(i in 1:72){
	for(j in 1:36){
		land_real[i,j]=land_geo[37-j,i]
	}
}

get_land_index <- function(land_real, geo){
	length_index = dim(geo)[1];
	index <- c();
	for (i in c(1:length_index)){
		if (land_real[(geo[i,1]+1),(geo[i,2]+1)] == 1){
			index = c(index,i);
		}
	}
	return(index);
}

filename = c("601m.mat","602m.mat","603m.mat","604m.mat",
	"605m.mat","606m.mat","607m.mat","608m.mat","609m.mat",
	"610m.mat","611m.mat","612m.mat")
land_index = c();
gap_index = c();

for (t in 1:length(filename)){
#for (t in 1:2){
	print(filename[t]);
	mfile <- readMat(filename[t]);
	geo <- matrix(unlist(mfile[1]),ncol=2);
	temp_index = get_land_index(land_real, geo);
#	print(temp_index); 
	land_index = c(land_index,(temp_index+sum(gap_index)));
	gap_index = c(gap_index,dim(geo)[1]);
}


#fname = c();
#for (t in c(1:3)){
#	fname_temp = c('6',toString(t),'.bin');
#	fname[t] = paste(fname_temp, collapse = '');
#}


write.table(land_index,file="landindex.csv")
write.table(land_index,file="gapindex.csv")

