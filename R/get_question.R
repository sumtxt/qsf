#' Extract question by ID
#'
#' Helper function to retrieve a single question by 
#' primary or secondary attribute value. 
#' 
#' @param qsf qsf file as R list
#' @param df Return \code{data.frame}? (default: TRUE)
#' @param id_name which question ID to use: "PrimaryAttribute" or "SecondaryAttribute"?
#' @param id Value of \code{id_name}. 
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' get_question(svy_qsf, id_name='PrimaryAttribute', id='Q1')
#' get_question(svy_qsf, id_name='SecondaryAttribute', id='sex')
#' 
#' 
#' @export
get_question <- function(qsf, df=TRUE, id_name, id){
	get_element(qsf=qsf, df=df, id_name=id_name, id=id)
	}

get_element <- function(qsf, df=TRUE, id_name, id){
	pos <- detect_index2(qsf,  'SurveyElements', key=id_name, value=id)
	l <- pluck(qsf, "SurveyElements", pos)
	if( df==TRUE){
		l <- sq_as_df(l)
		return( bind_rows(l, .id='QID') )
	} else {
		return(l) 
	}
	}

