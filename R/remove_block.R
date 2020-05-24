#' Remove a question block 
#' 
#' @param qsf qsf file as R list
#' @param id block ID (default: random ID)
#' 
#' @details
#' This function removes a survey block but the questions assigned to this block remain part of the survey. They can be attached to another/new survey block.
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' get_block_description(svy_qsf)
#' get_block_description(
#'  remove_block(svy_qsf, id='BL_npw6udfrvIt9oNT'))
#' 
#' 
#' @export
remove_block <- function(qsf,id){

	bll_pos <- detect_index2(qsf, 
			"SurveyElements", 
			key="Element", value="BL")

	bl_pos <- detect_index2(qsf, 
			"SurveyElements", bll_pos, "Payload", 
			key="ID", value=id)

	qsf <- assign_in(qsf, list(
			"SurveyElements", 
			bll_pos, 
			"Payload", 
			bl_pos), NULL )

	return(qsf)
	}
