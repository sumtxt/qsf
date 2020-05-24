#' Construct a controller for force / request response feature
#'
#' Helper function to enable "force a response" or "request a response" for survey questions. 
#' 
#' @param type "force", "request", or "no" (default: "force")
#'
#' @details 
#' This function is typically used in combination with \code{\link{update_question}}. See example.
#' 
#' @seealso 
#' \code{\link{update_question}}
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' # Enable 'Request Response' validation
#' # for the question on respondents' sex 
#' # and country of birth
#' 
#' qsf_mod <- update_question(svy_qsf, 
#'  id_name='SecondaryAttribute', 
#'  id=c('sex', 'born'), 
#'  Validation=construct_validation(type='request'))
#'
#'
#' @export
construct_validation <- function(type="force"){
	
	if(type == "force"){
		lst <- list(Settings=
				list(ForceResponse="ON", 
						 ForceResponseType="ON",
						 Type = "None"))
		}

	if(type == "request"){
		lst <- list(Settings=
			list(ForceResponse="RequestResponse", 
					 ForceResponseType="RequestResponse",
					 Type = "None"))
		}

  if(type == "no"){
		lst <- list(Settings=
				list(ForceResponse="OFF", 
						 ForceResponseType="ON",
						 Type = "None"))
		}
	
	return(lst)
	}