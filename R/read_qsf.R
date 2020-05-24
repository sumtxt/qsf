#' Import a QSF file 
#'
#' \code{read_qsf} reads a QSF file as a list 
#' 
#'
#' @param path file on disk
#' @param ... additional conversion arguments, see \code{\link[jsonlite]{read_json}}
#' 
#' @details  
#' This function is just a wrapper around \code{\link[jsonlite]{read_json}}. 
#' Any QSF file is a JSON file. 
#' 
#' @export
read_qsf <- function(path, ...) read_json(path, ...)