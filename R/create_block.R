#' Create a new question block 
#' 
#' @param qsf qsf file as R list
#' @param description description of survey block
#' @param id block ID (default: random ID)
#' 
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' get_block_description(svy_qsf)
#' get_block_description(
#' 	create_block(svy_qsf, "new block"))
#' 
#' @export
create_block <- function(qsf, description, id=NULL){

	if(is.null(id)) id <- paste0("BL_",make_qid())

	lst <-  list(list(Type = "Standard",
					SubType = "",
          Description = description,
          ID = id,
          BlockElements = list()))

	bl_pos <- detect_index2(qsf, "SurveyElements", 
			key="Element", value="BL")

	qsf <- modify_in(qsf, list(
			"SurveyElements", 
			bl_pos, 
			"Payload"), ~append(.x, lst) )

	return(qsf)

	}
