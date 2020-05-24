#' Create a new Qualtrics survey file 
#' 
#' Constructs a QSF file from a list of questions  
#' 
#' @param sq list of questions 
#' @param page_break add page break between each question? (default: FALSE)
#' 
#' 
#' @examples
#' data(svy_qsf)
#' 
#' sq <- get_questions(svy_qsf, df=FALSE)
#' qsf <- create_survey(sq, page_break=TRUE)
#' 
#' 
#' @export
create_survey <- function(sq, page_break=FALSE){
	if( has_element_sq(sq) ) sq <- list(sq)	
	sq <- keep(sq, has_element_sq)
	if (is.null(sq)) warning("No questions supplied.")
	new <- append_survey(qsf0, sq)
	qids <- flatten_chr(map(sq, ~pluck(., "Payload", "QuestionID")))
	if( length(qids)!=unique(length(qids)) ){
		warning("QuestionIDs are not unique in supplied question list.")
	}
	new <- attach_question(new, block_id='BL_npw6udfrvIt9oNT', 
		qids, page_break=page_break)
	return(new)
	}