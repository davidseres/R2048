get_EmptyFields <- function() {
	which(grid_2048 == 0, arr.ind = T)
}

get_OccupiedFields <- function() {
	which(grid_2048 > 0, arr.ind = T)
}

step_RandAdd <- function() {

	mat_EmptyFields <- get_EmptyFields()
	mat_OccupiedFields <- get_OccupiedFields()

	n_EmptyFields <- dim(mat_EmptyFields)[1]
	n_OccupiedFields <- dim(mat_OccupiedFields)[1]
	
	if(n_EmptyFields > 0) {
		idx_RandSet <- mat_EmptyFields[sample(1 : n_EmptyFields, 1), ]
		mat_RandAdd <- c(2,4)
		idx_ProbChoice <- (runif(1) > 0.8) + 1
		grid_2048[matrix(idx_RandSet, 1, 2)] <<- mat_RandAdd[idx_ProbChoice]
	}
	
}

step_Move <- function(UI) {

	mat_OccupiedFields <- get_OccupiedFields()
	grid_2048TEMP <- grid_2048

	switch(UI,
	
		left = {
		
			idx_NonEmptyRows <- which(rowSums(grid_2048) > 0)
			for(NE_Row in idx_NonEmptyRows) {
				
				mat_NonEmptyRow <- grid_2048[NE_Row, ]
				mat_NonEmptyRow <- c(	mat_NonEmptyRow[mat_NonEmptyRow > 0], 
										mat_NonEmptyRow[mat_NonEmptyRow == 0])
				idx_ToMerge <- which(diff(mat_NonEmptyRow) == 0)
				if(all(1:3 %in% idx_ToMerge)) idx_ToMerge <- c(1,3)
				for(iMerge in idx_ToMerge) {
					num_Merge <- sum(mat_NonEmptyRow[c(iMerge, iMerge + 1)])
					num_Points <<- num_Points + num_Merge
					mat_NonEmptyRow[c(iMerge, iMerge + 1)] <- c(num_Merge, 0)
				}

				grid_2048[NE_Row, ] <<- c(	mat_NonEmptyRow[mat_NonEmptyRow > 0], 
										mat_NonEmptyRow[mat_NonEmptyRow == 0])
				
			}
			
		},
		
		up = {
		
			idx_NonEmptyCols <- which(colSums(grid_2048) > 0)
			for(NE_Col in idx_NonEmptyCols) {
			
				mat_NonEmptyCol <- grid_2048[, NE_Col]
				mat_NonEmptyCol <- c(	mat_NonEmptyCol[mat_NonEmptyCol > 0], 
										mat_NonEmptyCol[mat_NonEmptyCol == 0])				
				idx_ToMerge <- which(diff(mat_NonEmptyCol) == 0)
				if(all(1:3 %in% idx_ToMerge)) idx_ToMerge <- c(1,3)
				for(iMerge in idx_ToMerge) {
					num_Merge <- sum(mat_NonEmptyCol[c(iMerge, iMerge + 1)])
					num_Points <<- num_Points + num_Merge
					mat_NonEmptyCol[c(iMerge, iMerge + 1)] <- c(num_Merge, 0)
				}			
				
				grid_2048[, NE_Col] <<- c(	mat_NonEmptyCol[mat_NonEmptyCol > 0], 
										mat_NonEmptyCol[mat_NonEmptyCol == 0])
										
			}
			
		},
		
		right = {
		
			idx_NonEmptyRows <- which(rowSums(grid_2048) > 0)
			for(NE_Row in idx_NonEmptyRows) {
				
				mat_NonEmptyRow <- grid_2048[NE_Row, ]
				mat_NonEmptyRow <- c(	mat_NonEmptyRow[mat_NonEmptyRow == 0], 
										mat_NonEmptyRow[mat_NonEmptyRow > 0])
				idx_ToMerge <- which(diff(mat_NonEmptyRow) == 0)
				if(all(1:3 %in% idx_ToMerge)) idx_ToMerge <- c(1,3)
				for(iMerge in rev(idx_ToMerge)) {
					num_Merge <- sum(mat_NonEmptyRow[c(iMerge, iMerge + 1)])
					num_Points <<- num_Points + num_Merge
					mat_NonEmptyRow[c(iMerge, iMerge + 1)] <- c(0, num_Merge)
				}

				grid_2048[NE_Row, ] <<- c(	mat_NonEmptyRow[mat_NonEmptyRow == 0], 
										mat_NonEmptyRow[mat_NonEmptyRow > 0])
				
			}
			
		},
		
		down = {
		
			idx_NonEmptyCols <- which(colSums(grid_2048) > 0)
			for(NE_Col in idx_NonEmptyCols) {
			
				mat_NonEmptyCol <- grid_2048[, NE_Col]
				mat_NonEmptyCol <- c(	mat_NonEmptyCol[mat_NonEmptyCol == 0], 
										mat_NonEmptyCol[mat_NonEmptyCol > 0])				
				idx_ToMerge <- which(diff(mat_NonEmptyCol) == 0)
				if(all(1:3 %in% idx_ToMerge)) idx_ToMerge <- c(1,3)
				for(iMerge in rev(idx_ToMerge)) {
					num_Merge <- sum(mat_NonEmptyCol[c(iMerge, iMerge + 1)])
					num_Points <<- num_Points + num_Merge
					mat_NonEmptyCol[c(iMerge, iMerge + 1)] <- c(0, num_Merge)
				}			
				
				grid_2048[, NE_Col] <<- c(	mat_NonEmptyCol[mat_NonEmptyCol == 0], 
										mat_NonEmptyCol[mat_NonEmptyCol > 0])
										
			}
			
		}
		
	)
	
	if(!all(grid_2048TEMP == grid_2048)) step_RandAdd()

	#grid_2048
	
}

initialize <- function() {

	grid_2048 <<- array(0, dim = c(4, 4))
	mat_EmptyFields <- get_EmptyFields()
	n_EmptyFields <- dim(mat_EmptyFields)[1]
	bln_StopCondition <<- F
	op <- par(no.readonly = T)
	num_Points <<- 0
	
	idx_Start <- mat_EmptyFields[sample(1 : n_EmptyFields, 1), ]
	grid_2048[matrix(idx_Start, 1, 2)] <<- 2^(sample(1 : 2, 1))

	step_RandAdd()

	#grid_2048

}

stop_Condition <- function() {

	mat_EmptyFields <- get_EmptyFields()
	mat_OccupiedFields <- get_OccupiedFields()

	n_EmptyFields <- dim(mat_EmptyFields)[1]
	n_OccupiedFields <- dim(mat_OccupiedFields)[1]
	
	min(n_EmptyFields, 1)
	bln_GridFull <- !as.logical(n_EmptyFields)

	mat_MergePossibility <- rbind(diff(grid_2048), diff(t(grid_2048)))
	bln_GridLocked <- !any(mat_MergePossibility == 0)
	
	bln_StopCondition <<- bln_GridFull & bln_GridLocked

}

plot_Grid2048 <- function() {

	plot(0:4,0:4, type = "n", xaxt = "n", xaxs = "i" , yaxt = "n", yaxs = "i", xlab = paste("Points: ", num_Points), ylab = "")
	grid(4)
	grid_2048ToPlot <- grid_2048
	grid_2048ToPlot[grid_2048ToPlot == 0] <- ""
	y_AxisReverse <- nrow(grid_2048):1
	for(iRow in 1:nrow(grid_2048)) {
		for(iCol in 1:ncol(grid_2048)) {
			text(iCol - 0.5, y_AxisReverse[iRow] - 0.5, grid_2048ToPlot[iRow, iCol], cex = 1.5)
		}
	}

}
