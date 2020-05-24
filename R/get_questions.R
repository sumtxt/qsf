#' Extract questions from QSF file 
#'
#' @param qsf qsf file as R list
#' @param df convert to a \code{data.frame}? (default: TRUE)
#' 
#' @details   
#' This function extracts all questions from a QSF file and can convert those to a \code{data.frame}.
#'
#' @return If \code{df=FALSE} the function pulls all questions from the qsf without further modifications. If \code{df=TRUE} the function returns a \code{data.frame} with columns as listed below. Note, that the conversion into a \code{data.frame} does not preserve information such as display and skip logic. 
#' 
#' \itemize{
#' 	QID: Question ID (newly generated based on the sequence of questions in \code{qsf})
#' 	ID: Choice ID (if Class="Question" then ID=0)
#'  Display : Displayed text 
#' 	Class: One of three values: Question, Choices, Answers.
#' 	Language : The language code of the question, choice or answer.
#' 	QuestionType : Value from [['Payload']][['QuestionType']]
#' 	PrimaryAttribute: Value from [['PrimaryAttribute']]
#' 	SecondaryAttribute: Value from [['SecondaryAttribute']]
#' 	TextEntry: Text-entry allowed?
#' 	}
#' 
#' @examples 
#' data(svy_qsf)
#' 
#' get_questions(svy_qsf)
#' 
#' 
#' 
#' @export
get_questions <- function(qsf, df=TRUE){
	l <- pluck(qsf, 'SurveyElements')
	if( df==TRUE){
		l <- map( keep(l, has_element_sq ), sq_as_df ) 
		return( bind_rows(l, .id='QID') )
	} else {
		return( keep(l, has_element_sq ) ) 
	}
	}

sq_as_df <- function(l){
	en <- get_q( pluck(l, "Payload") ) 
	en <- add_column(en, Language='EN')
	lng <- pluck(l, "Payload", "Language")
	df <- bind_rows( map(lng, ~get_q(.)), .id="Language")
	df <- bind_rows(en,df) 
	or <- get_o( pluck(l, "Payload") ) 
	df <- full_join(df, or, by=c("value", "Class"))
	df <- select(df, "ID", everything(), -"value")
	df <- add_column(df, 
					 QuestionType=pluck(l, "Payload", "QuestionType"), 
					 PrimaryAttribute=pluck(l, "PrimaryAttribute"), 
					 SecondaryAttribute=pluck(l, "SecondaryAttribute"))
	return(df)
	}

# Get question, choices, answers
get_q <- function(l){
	df <- tibble(value="0", Display=pluck(l, "QuestionText"), Class="Question")
	choices <- bind_rows( map(pluck(l, "Choices"), as_tibble), .id='value') 
	choices <- mutate(choices, Class='Choices')
	if( vec_depth(pluck(l, "Answers"))>3 ) {
		warning("Skipping Answers for special matrix questions.")
		return(bind_rows(df, choices))
	}
	answers <- bind_rows( map(pluck(l, "Answers"), as_tibble), .id='value') 
	answers <- mutate(answers, Class='Answers')
	df <- bind_rows(df, choices, answers)
	return(df)
	}

# Get order of choices and answers
get_o <- function(l){
	df <- tibble(value="0", ID="0", Class="Question")
	choices <- map(pluck(l, "ChoiceOrder"), as.character)
	answers <- map(pluck(l, "AnswerOrder"), as.character)
	choices <- bind_rows(map(choices, as_tibble), .id='ID')
	answers <- bind_rows(map(answers, as_tibble), .id='ID')
	choices <- mutate(choices, Class='Choices')
	answers <- mutate(answers, Class='Answers')
	df <- bind_rows(df, choices, answers)
	return(df)
	}
