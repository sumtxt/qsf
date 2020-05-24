#' Append survey questions to an existing survey
#'
#' Transplanting survey elements (e.g., questions) from one survey to another 
#'
#' @param qsf qsf file as R list
#' @param sq list of survey questions (elements)
#' 
#' @details 
#' Note that new survey questions also need to be attached to a question block (see \code{\link{attach_question}}).
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' # Copy Q1 from survey 
#' q <- get_question(svy_qsf, id_name='PrimaryAttribute', id='Q1', df=FALSE)
#' 
#' # Remove Q1 from survey 
#' qsf <- remove_question(svy_qsf, id_name='PrimaryAttribute', id='Q1')
#' 
#' # Reappend Q1 to survey
#' qsf <- append_survey(qsf, list(q))
#'  
#' 
#' @export
append_survey <- function(qsf, sq){
	modify_in(qsf, 'SurveyElements', ~append(.x, sq) )
	}