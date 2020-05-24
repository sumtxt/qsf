#' Data frame with two survey questions
#'
#' A data frame representing two binary choice survey questions in three languages (English, German, and French). 
#'
#' @format A data frame with 12 rows and 7 variables.
#' \describe{
#'  \item{QuestionType}{Qualtrics question type}
#'  \item{QuestionDescription}{Question description}
#'  \item{QuestionID}{Question ID}
#'  \item{Language}{Language code}
#'  \item{QuestionText}{Question text}
#'  \item{ChoiceID}{Choice ID}
#'  \item{Choices}{Choice label}
#' }
#' 
#' @seealso 
#' \code{\link{svy_qsf}}s 
#' 
"svy_df"

#' A qsf file as R list with two survey questions
#'
#' The list object representing the qsf file includes two binary choice survey questions in three languages (English, German, and French). Includes the same questions as \code{\link{svy_df}}.
#'
#' @format list object
#' @seealso 
#' \code{\link{svy_df}} 
#' 
"svy_qsf"