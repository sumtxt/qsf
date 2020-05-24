#' Export survey as a QSF file 
#'
#' \code{write_qsf} writes a QSF file based on a 
#' survey and performs some formatting checks
#'
#' @param qsf qsf file as R list
#' @param path file on disk
#' @param ... additional conversion arguments, see \code{\link[jsonlite]{write_json}}
#' 
#' @details  
#' This function is just a wrapper around \code{\link[jsonlite]{write_json}}. 
#' Any QSF file is a JSON file. 
#' 
#' 
#' @examples 
#' data(sq_qsf)
#' 
#' write_qsf(sq_qsf, path=tempfile())
#' 
#' @export
write_qsf <- function(qsf, path, ...){
	if( check_survey(qsf) ) {
		write_json(qsf, path=path, auto_unbox=TRUE, null='null', ...)
		}
	}


check_survey <- function(qsf){

	m <- get_questions(qsf, df=FALSE)

	# Check uniqueness of the IDs
	qid <- map(m, ~pluck(.,"Payload", "QuestionID"))
	qid <- unlist(qid)
	if( n_distinct(qid)!=length(qid) ){
		stop("QuestionID is not unique!")
		}

	# Proper ID Structure 
	id3 <- str_detect(qid, "^QID[0-9]*$")
	id2 <- str_detect(qid, "^[A-Za-z][0-9]*\\.[0-9]*$")
	id1 <- str_detect(qid, "^[A-Za-z][0-9]*$")
	if( sum(id3 + id2 + id1)!=length(qid) ){
		stop("QuestionID not compatible!")
		}

	return(TRUE)
}