#' Construct a controller for display logic 
#'
#' Helper function to construct conditions when a question is displayed.
#' 
#' @param ... key-value tuple (see below)
#'
#' @details 
#' Each condition comes as a tuple. The first condition takes the key "0" and all subsequent conditions are numbered. The value is a named list with the following elements: \code{qid} (QuestionID), \code{operator} ("Selected", "NotSelected"), \code{choice_id}. If there is more than one condition, all tuples other than that with key="0" must also include the parameter \code{conjuction} with the value "OR" or "AND". 
#' 
#' @seealso 
#' \code{\link{update_question}}
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' # Display question "born" only 
#' # when respondent selected 1 or 2 
#' # for the first question 
#' 
#' qsf_mod <- update_question(svy_qsf, 
#'  id_name='SecondaryAttribute', 
#'  id='born', 
#'  DisplayLogic=construct_displaylogic( 
#'     `0`=list(qid="Q1", operator="Selected", choice_id="1"), 
#'     `1`=list(qid="Q1", operator="Selected", choice_id="2", conjuction='Or'))
#'  )
#' 
#' @export
construct_displaylogic <- function(...){

  lst <- dots_list( ..., .homonyms = "error",  .check_assign = TRUE)
  conditions <- map(lst, ~exec(create_displaylogic_condition, !!!.))
  conditions[["Type"]] <- "If"
  lst <- list(`0` =conditions, 
     Type = "BooleanExpression",
     inPage = FALSE)

  return(lst)
  }


create_displaylogic_condition <- function(qid, 
  operator, choice_id, conjuction=NULL, 
  path='SelectableChoice', ...){
  lst <- list(
    LogicType = "Question",
    QuestionID = qid,
    QuestionIsInLoop = "no",
    ChoiceLocator = question_path(qid=qid, 
      choice_id=choice_id, path=path),
    Operator = operator,
    QuestionIDFromLocator = qid,
    LeftOperand = question_path(qid=qid, 
      choice_id=choice_id, path=path),
    Type = "Expression")
  lstx <- dots_list( ..., .homonyms = "error",  
      .check_assign = TRUE)
  lst <- c(lst,lstx)
  if( !is.null(conjuction) ){
    lst[['Conjuction']]  <- conjuction
  }
  return(lst)
  } 

