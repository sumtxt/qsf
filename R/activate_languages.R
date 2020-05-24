#' Activates additional language
#'
#' To display a translation of a question in the survey, activate the respective language
#' 
#' @param qsf qsf file as R list
#' @param languages vector of language codes
#' 
#' 
#' @return 
#' qsf file as R list
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' qsf <- activate_languages(svy_qsf, 
#' 	languages=c("DE", "FR"))
#'  
#'  
#' @export
activate_languages <- function(qsf, languages){
	K <- length(languages)
	l <- vector(mode='list', length=K)
	names(l) <- languages
	l <- lapply(l, function(x) list() ) 
	qsf <- update_question(qsf,  
			id_name='Element', 
			id='SO', 
			AvailableLanguages = l)
	return(qsf)
	}