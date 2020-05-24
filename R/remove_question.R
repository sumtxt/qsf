#' Remove a question
#' 
#' @param qsf qsf file as R list
#' @param id_name which question ID to use: "PrimaryAttribute" or "SecondaryAttribute"?
#' @param id Value of \code{id_name}. 
#' 
#' @details
#' This function removes a question but leaves it attached to a question block. 
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' remove_question(svy_qsf, id_name='PrimaryAttribute', id='Q1')
#' remove_question(svy_qsf, id_name='SecondaryAttribute', id='sex')
#' 
#' 
#' @export
remove_question <- function(qsf,id_name, id){
	pos <- detect_index2(qsf,  'SurveyElements', key=id_name, value=id)
	qsf[['SurveyElements']][[pos]] <- NULL
	return(qsf)
	}