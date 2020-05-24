#' Attaches a question to a block 
#'
#' This function appends a question to an existing survey block.
#' 
#' @param qsf qsf file as R list
#' @param block_id ID of question block 
#' @param qid QuestionID 
#' @param page_break Add a question break? (Default: FALSE)
#' 
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' get_block_description(svy_qsf)
#' qsf <- remove_block(svy_qsf, 'BL_npw6udfrvIt9oNT')
#' qsf <- create_block(qsf, 'demographics', id='BL_C5Z2OqNL6no9APV')
#' 
#' qsf <- attach_question(qsf, 
#' 	block_id="BL_C5Z2OqNL6no9APV", 
#'	qid=c("Q1", "Q2")) 
#' 
#' get_block_description(qsf)
#' 
#' 
#' @export
attach_question <- function(qsf, block_id, qid, page_break=FALSE){
	if(page_break==TRUE){
		el <- lapply(qid, function(q) list(list("Type"="Question", "QuestionID"=q), 
			list("Type"="Page Break")) ) 
		el <- flatten(el)
		el[[length(el)]] <- NULL
	} else { 
		el <- lapply(qid, function(q) list("Type"="Question", "QuestionID"=q) )	
	}
	bll_pos <- detect_index2(qsf, "SurveyElements", 
			key="Element", value="BL")
	bl_pos <- detect_index2(qsf, "SurveyElements", bll_pos, "Payload", 
			key="ID", value=block_id)
	qsf <- modify_in(qsf, list(
			"SurveyElements", 
			bll_pos, 
			"Payload", 
			bl_pos, 
			"BlockElements"), ~append(.x, el) )
	return(qsf)
	}

#' @export
detect_index2 <- function(.x, ..., key, value){
	detect_index( pluck(.x, ...),  
			 ~ has_key_value(., key=key, value=value) )
	}