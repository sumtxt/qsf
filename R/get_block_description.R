#' Display information on question blocks
#'
#' @param qsf qsf file as R list
#'
#' @return
#' Returns a \code{tibble} with a row for each survey block, the block id, block description and the number of questions.
#' 
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' get_block_description(svy_qsf)
#' 
#' @export
get_block_description <- function(qsf){
	sq <- get_element(qsf, df=FALSE, 
		id_name="Element", id="BL")
	df <- map_df(pluck(sq, "Payload"), get_bldesc)
	return(df)
	}

get_bldesc <- function(l){
	df <- tibble(type=l[['Type']],
		description=l[['Description']],
		id = l[['ID']],
		k = length(l[['BlockElements']]))
	return(df)
	}