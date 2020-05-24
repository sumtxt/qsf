#' Attaches a skip logic to a question
#'
#' Skip logic allows to send respondents to the end of the block or the end of the survey based on how they answered a question. 
#' 
#' @param qsf qsf file as R list
#' @param qid question id 
#' @param block_id block id 
#' @param choice_id choice id 
#' @param condition either "Selected", "NotSelected", "Displayed",  "NotDisplayed"
#' @param skipto either "ENDOFSURVEY" or "ENDOFBLOCK"
#' 
#' @details 
#' To find the ID of a question and the block ID use \code{\link{get_block_description}} and \code{\link{get_questions}}.
#' 
#' @return 
#' qsf file as R list
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' # Respondents that select 'Male' (choice_id=1)
#' # are send to the end of the survey.
#' 
#' svy_mod <- add_skiplogic(svy_qsf, 
#'  qid='Q1',
#'  block_id='BL_npw6udfrvIt9oNT',
#'  choice_id=1,
#'  condition="Selected",
#'  skipto="ENDOFSURVEY")
#'  
#' @export
add_skiplogic <- function(qsf, qid, block_id, 
	condition, choice_id, skipto){

	if(!(skipto %in% c("ENDOFBLOCK","ENDOFSURVEY"))) {
		stop("skipto must be either 
			ENDOFSURVEY or ENDOFBLOCK.")
	}

	if(!(condition %in% c("Selected", "NotSelected", 
			"Displayed",  "NotDisplayed"))) {
		stop("condition is not a valid value.")
	}

	sq <- get_element(qsf, df=FALSE, 
		id_name="Element", id="BL")
	qsf <- remove_question(qsf, 
		id_name ="Element", id="BL")

	bl_pos <- detect_index2(sq, "Payload", 
		key="ID", value=block_id)
	bl <- pluck(sq, "Payload", bl_pos)
	pos <- detect_index2(bl, "BlockElements", 
		key="QuestionID", value=qid)

	lst <- pluck(sq, "Payload", bl_pos, "BlockElements", pos)
  lst[["SkipLogic"]] <- list(create_skiplogic(qid=qid, condition=condition, 
			choice_id=choice_id, skipto=skipto))

	pluck(sq, "Payload", bl_pos, 
		"BlockElements", pos) <- lst
	qsf <- append_survey(qsf, list(sq))
	return(qsf)
	}

create_skiplogic <- function(qid, condition, choice_id, skipto){
  qpath <- question_path(qid=qid, choice_id=choice_id)
  id <- floor(runif(1, min=1, max=9999999999))
  lst <- list(
    SkipLogicID=id, 
    ChoiceLocator=qpath, 
    Condition=condition, 
    SkipToDestination=skipto, 
    Locator=qpath,
    QuestionID=qid)
  return(lst)
  }

question_path <- function(qid, choice_id, path='SelectableChoice'){
  return(paste0("q://",qid, "/",path, "/", choice_id))
  }

