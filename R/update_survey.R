#' Update survey settings 
#' 
#' @param qsf qsf file as R list
#' @param ... key-value tuples (see details)
#' 
#' @details
#' The list belows catalogs some of the common keys that control the behavior of a survey. The function accepts all key-value tuples. 
#' 
#' \itemize{
#'  \item BallotBoxStuffingPrevention : 'true' or 'false' (string)
#'  \item SurveyTitle : Name of the survey appearing in the browser as the window or tab title. 
#'  \item SurveyMetaDescription : Search engines and social media services use this description. 
#'  \item Header : A string with JavaScript code placed in the header of the survey. 
#' }
#'  
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' # Enable ballot box stuffing prevention 
#' qsf <- update_survey(qsf, 
#' 	 BallotBoxStuffingPrevention='true')
#' 
#' 
#' 
#' 
#' @export
update_survey <- function(qsf, 
	id_name, id, ...){
	update_question(qsf, id_name='Element', id='SO', ...)
	}

