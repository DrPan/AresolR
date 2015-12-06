find_land <- function(filename){
	coast_line_mat <- readMat('.\coast_line.mat');
	coast_line = matrix(unlist(coast_line_mat[1]),ncol = 720, byrow = TRUE);
}

horizontal_move <- function(coast_line, center_x, center_y, land_map){
	if (coast_line[center_x-1, center_y] ==0){
		coast_line[center_x-1, center_y] = 1;
		horizontal_move(center_x-1, center_y);
	} elseif (coast_line[center_x+1, center_y] ==0){
		coast_line[center_x+1, center_y] = 1;
		horizontal_move(center_x+1, center_y);
	} else{
	return(coast_line);
	}
}

vertical_move <- function(coast_line, center_x, center_y, land_map){
	if (coast_line[center_x, center_y-1] ==0){
		coast_line[center_x, center_y-1] = 1;
		horizontal_move(center_x, center_y-1);
	} elseif (coast_line[center_x, center_y+1] ==0){
		coast_line[center_x, center_y+1] = 1;
		horizontal_move(center_x, center_y+1);
	} else{
	return(coast_line);
	}
}

