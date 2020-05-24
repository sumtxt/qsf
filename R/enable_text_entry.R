#' Enable "Allow text entry" for a choice  
#'
#' @param qsf qsf file as R list
#' @param id_name which question ID to use: "PrimaryAttribute" or "SecondaryAttribute"?
#' @param id Value of \code{id_name}. 
#' @param choice_id Choice ID 
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' enable_text_entry(svy_qsf, 
#'  id_name='SecondaryAttribute', 
#'  id='sex', choice_id=2)
#' 
#' 
#' @export
enable_text_entry <- function(qsf, id_name, id, choice_id){
	sq <- get_question(qsf, df=FALSE, id_name=id_name, id=id)
	qsf <- remove_question(qsf, id_name=id_name, id=id)
	choice_id <- as.character(choice_id)
	lst <- pluck(sq, "Payload", "Choices", choice_id)
	lst[['TextEntry']] <- 'true'
	pluck(sq, "Payload", "Choices", choice_id) <- lst 
	qsf <- append_survey(qsf, list(sq))
	return(qsf)
	}