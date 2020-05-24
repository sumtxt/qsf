#' @importFrom jsonlite write_json read_json
#' @importFrom rlang dots_list exec enquo
#' @importFrom stringr str_detect
#' @importFrom tibble tibble as_tibble deframe add_column
#' @import dplyr
#' @import purrr
NULL

make_qid <- function(k=15){
	id <- sample(c(LETTERS, letters,1:9), size=k)
	return(paste0(id, collapse=""))
}

has_key_value <- function(l, key, value) {
	if ( is.null(pluck(l, key)) ) return(FALSE)
	if ( pluck(l, key)==value ) return(TRUE)
	else return(FALSE)
	}

has_element_sq <- function(l) {
	has_key_value(l, key="Element", value='SQ')
	}
