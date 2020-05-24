#' Update attributes of a question
#' 
#' Versatile function to change the behavior of a question and the survey. 
#' 
#' @param qsf qsf file as R list
#' @param id_name which question ID to use: "PrimaryAttribute" or "SecondaryAttribute"?
#' @param id Value of \code{id_name}. 
#' @param ... key-value tuples (see details)
#' 
#' @details
#' The list below catalogs some of common keys that control the behavior of a question.  The function accepts all key-value tuples. 
#' 
#' \itemize{
#'  \item Validation: A list controlling the validation. Use \code{\link{construct_validation()}} to construct. 
#'  \item DisplayLogic : A list controlling the display logic. Use \code{\link{construct_displaylogic()}} to construct.
#'  \item DataExportTag : A string without whitespace.
#'  \item QuestionJS : A string with JavaScript code attached to the question. 
#'  \item Selector and SubSelector: 
#'   \tabular{lll}{
#'    Selector \tab SubSelector \tab Choice display \cr
#'    MAVR \tab 'TX' \tab Multiple choice \cr
#'    SAVR \tab 'TX' \tab Single choice \cr
#'    DL \tab \tab Dropdown \cr
#'    SB \tab \tab Select box \cr
#'    MSB \tab \tab Multi select box \cr
#'   }
#' }
#'  
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' # Change data export tag 
#' qsf <- update_question(qsf, 
#'		id_name='SecondaryAttribute', 
#'		id='sex', 
#'		DataExportTag='gender')
#'
#' # Convert to multiple choice 
#' qsf <- update_question(qsf, 
#'		id_name='SecondaryAttribute', 
#'		id='sex', 
#'		Selector='MAVR',
#'		SubSelector='TX')
#'
#' # Forced choice for both questions 
#' qsf_mod <- update_question(svy_qsf, 
#'  id_name='SecondaryAttribute', 
#'  id=c('sex', 'born'), 
#'  Validation=construct_validation(type='force'))
#' 
#' 
#' 
#' 
#' 
#' @export
update_question <- function(qsf, 
	id_name, id, ...){
	if( is.null(id) ){
		warnings("No ID suppplied. Modify to all questions.")
		qs <- get_questions(qsf, df=FALSE)
		id <- flatten_chr(map(qs, ~pluck(., "PrimaryAttribute")))
		id_name <- 'PrimaryAttribute'
	}
	if(length(id)==1){
		qsf <- update_question_(qsf=qsf,
				id_name=id_name, id=id, ...)
	} else {
		for(x in id){
			qsf <- update_question_(qsf=qsf, 
				id_name=id_name, id=x, ...) 
		}
	}
	return(qsf)
	}

update_question_ <- function(qsf, 
	id_name, id, ...){
	l <- get_question(qsf, df=FALSE, id_name=id_name, id=id)
	qsf <- remove_question(qsf, id_name=id_name, id=id)
	l <- set_question_attributes(l, ...)
	qsf <- append_survey(qsf, list(l))
	return(qsf)
	}

