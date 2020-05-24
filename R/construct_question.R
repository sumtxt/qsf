#' Constructs a survey question from a data frame 
#'
#' This function takes a nested \code{tibble} representing a survey question and converts it into a survey questions that can be added to a qsf file using the \code{\link{append_survey()}} function. The function does the opposite of \code{\link{get_questions()}}.
#' 
#' @param df \code{tibble}
#' @param question_text variable name (unquoted) with the question text 
#' @param choices variable name (unquoted) with the choice \code{tibble}  (default: NULL)
#' @param answers variable name (unquoted) with the answer \code{tibble} (default: NULL)
#' @param language variable name (unquoted) with the language code
#' @param master language code of master language (default: "EN")
#' @param question_id question ID (default: Q1)
#' @param primary_attribute primary attribute string (default: Q1)
#' @param secondary_attribute secondary attribute string (default: "new question")
#' @param data_export_tag data export tag (default: Q1)
#' @param question_description question description 
#' @param question_type either "MC", "TE", "DB", or "Matrix"
#' 
#' @details  
#' The argument \code{choice} can be left empty (NULL) and will be ignored if \code{question_type='DB'}. The argument \code{answer} must be supplied if \code{question_type='Matrix'}. 
#' 
#' The \code{tibble} for \code{choice} and \code{answer} must include two: a column with numeric IDs determining the display order and the a column with the labels for each choice. The column names are ignored.
#' 
#' For surveys constructed via the online editor, \code{primary_attribute} and \code{question_id} are typically identical and can not be modified by the user. The argument \code{question_id} must be unique within the survey and follow one of three schemes: 
#' \itemize{
#'  \item Sequential numbering: Q1, Q2, ...
#'  \item Block numbering: Q1.1, Q1.2... (Block 1); Q2.1, Q2.2, ... (Block 2)
#'  \item Internal ID numbering: QID1, QID2, ...
#' }
#' For sequential and block numbering "Q" can be replaced by any other capitalized letter from the Latin alphabet. When using \code{\link{write_qsf}} these conventions are checked.  
#' 
#' \if{html}{\figure{screenshot.png}{options: width=100 alt="Screenshot Qualtrics online editor"}}
#' 
#' The argument \code{secondary_attribute} controls the question label shown in the online editor. In the picture above, "sex" is the question label. The argument data_export_tag controls the name of the variable in the csv file with the collected survey data. When constructing a survey via the online editor, the \code{data_export_tag} is identical to \code{question_id} as long as the user doesn't modify. In the picture above, the data export tag is "Q1". 
#' 
#' The argument \code{question_type} must be one of four options: 
#' \itemize{
#'  \item MC : Multiple/single choice question 
#'  \item Matrix : Multiple choice, multiple answer question
#'  \item TE : Text entry question
#'  \item DB : Descriptive text only (no choices/answers)
#' }
#' 
#' Please note, that all questions with \code{question_type=MC} are single choice by default. See \code{\link{update_question}} for examples on how to change that. 
#' 
#' 
#' 
#' @return 
#' List of survey elements.
#' 
#' @examples 
#' q <- tibble(
#' 		QuestionText="How can we contact you?",
#' 		Choices = list(tibble(
#' 					id=c(1,2),
#' 					label=c("Email","SMS"))),
#' 		Language = "EN"
#' 		)
#' 
#' sq <- construct_question(q, 
#'     question_text=QuestionText, 
#'     choices=Choices, 
#'     language=Language, 
#'     master="EN",  
#'     question_id="Q1",
#'     primary_attribute="Q1",
#'     secondary_attribute="contact",
#'     data_export_tag="contact", 
#'     question_description="Contact question", 
#'     question_type = "MC") 
#' 
#' qsf <- create_survey(sq)
#' 
#' 
#' @export
construct_question <- function(df, 
		question_text, 
		language, 
		choices=NULL, 
		answers=NULL, 
		master="EN", 
		question_id="Q1", 
		primary_attribute="Q1", 
		secondary_attribute="New question", 
		data_export_tag="Q1",
		question_description="first_question",
		question_type="MC"){

	question_text_ = enquo(question_text)
	choices_ = enquo(choices)
	answers_ = enquo(answers)
	language_ = enquo(language)

	if( length(master)!=1 ) {
		stop("Argument master must be of length 1.")
	}

	if( !(question_type %in% c("MC","TE","DB","Matrix")) ) {
		stop("Argument question_type must be either MC, TE, DB, or Matrix.")
	}

	sq <- qsf:::sq0

	if(question_type =="MC"){

		df_master <- filter(df, !!language_==master) %>% 
			do(lst=sq_as_lst( 
				question_text=pull(., !!question_text_),  
				choices=pull(., !!choices_), 
				choice_order=TRUE)) 

		df_master <- flatten(deframe(df_master))

		df_slave <- filter(df, !!language_!=master) %>% 
			group_by(!!language_) %>% 
			do(lst=sq_as_lst( 
						question_text=pull(., !!question_text_),  
						choices=pull(., !!choices_)))

		df_slave <- deframe(df_slave)

	} 

	if(question_type =="Matrix"){

		df_master <- filter(df, !!language_==master) %>% 
			do(lst=sq_as_lst( 
				question_text=pull(., !!question_text_),  
				choices=pull(., !!choices_), 
				answers=pull(., !!answers_), 
				choice_order=TRUE, 
				answer_order=TRUE)) 

		df_master <- flatten(deframe(df_master))

		df_slave <- filter(df, !!language_!=master) %>% 
			group_by(!!language_) %>% 
			do(lst=sq_as_lst( 
						question_text=pull(., !!question_text_),  
						choices=pull(., !!choices_), 
						answers=pull(., !!answers_)))

		df_slave <- deframe(df_slave)

	}

	if(question_type %in% c("TE", "DB")){

		df_master <- filter(df, !!language_==master) %>% 
			do(lst=sq_as_lst( 
				question_text=pull(., !!question_text_))) 

		df_master <- flatten(deframe(df_master))

		df_slave <- filter(df, !!language_!=master) %>% 
			group_by(!!language_) %>% 
			do(lst=sq_as_lst( 
						question_text=pull(., !!question_text_)))

		df_slave <- deframe(df_slave)

	}

	if(question_type =="MC"){

		sq <- set_question_attributes(sq, 
				Selector="SAVR", SubSelector="TX", 
				QuestionType="MC")

		} 

	if(question_type =="TE"){

		sq <- set_question_attributes(sq, 
				Selector="SL", QuestionType="TE")

		} 

	if(question_type =="DB"){
	
		sq <- set_question_attributes(sq, 
			Selector="TB", QuestionType="DB")

		} 

	if(question_type =="Matrix"){
	
		sq <- set_question_attributes(sq, 
				Selector="Likert", SubSelector="SingleAnswer", 
				QuestionType="Matrix")

		} 

	sq <- invoke(set_question_attributes, df_master, sq) 
	sq[["Payload"]][["Language"]] <- df_slave 

	sq <- set_question_attributes(sq, 
			QuestionID=question_id, 
			DataExportTag=data_export_tag,
			QuestionDescription=question_description) 

	sq[['PrimaryAttribute']] <- primary_attribute
	sq[['SecondaryAttribute']] <- secondary_attribute

	return(sq)
	}

summarize_choices <- function(choices){
	l <- as.list(deframe(flatten_df(choices)))
	l <- lapply(l, function(el) list("Display"=el) )
	return(l)
	}

sq_as_lst <- function(
		question_text, 
		choices=NULL,
		answers=NULL,
		choice_order=FALSE,
		answer_order=FALSE){ 

		lst <- list("QuestionText"=question_text)

		if( !is.null(choices) ){
			lst[["Choices"]]=summarize_choices(choices)
			if(choice_order==TRUE){
				lst[["ChoiceOrder"]] <- as.list(names(lst[["Choices"]]))
			}
		}
		
		if( !is.null(answers) ){
			lst[["Answers"]]=summarize_choices(answers)
			if(answer_order==TRUE){
				lst[["AnswerOrder"]] <- as.list(names(lst[["Answers"]]))
			}
		}
		return(lst)
		}	

set_question_attributes <- function(sq, ...){
	lst <- dots_list( ..., .homonyms = "error",  
		.check_assign = TRUE)
	for(k in 1:length(lst)){
		sq[["Payload"]][[names(lst)[k]]] <- NULL
	}
	sq <- modify_in(sq, "Payload", ~append(., lst))
	return(sq)
	}