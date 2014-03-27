start_2048 <- function() {

	source("R2048_functions.r")
	
	initialize()
	plot_Grid2048()
	
	while(!bln_StopCondition) {
	
		UI <- readline()
		UI_trans <- switch(UI, w="up", s="down", a="left", d="right", NULL)
		if(!is.null(UI_trans)) {
			step_Move(UI_trans)
			plot_Grid2048()
			stop_Condition()
		}
	
	}
	
	mtext("Game Over", cex = 2, col = "red")
	return("Game Over")

}
