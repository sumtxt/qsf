% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_question.R
\name{update_question}
\alias{update_question}
\title{Update attributes of a question}
\usage{
update_question(qsf, id_name, id, ...)
}
\arguments{
\item{qsf}{qsf file as R list}

\item{id_name}{which question ID to use: "PrimaryAttribute" or "SecondaryAttribute"?}

\item{id}{Value of \code{id_name}.}

\item{...}{key-value tuples (see details)}
}
\description{
Versatile function to change the behavior of a question and the survey.
}
\details{
The list below catalogs some of common keys that control the behavior of a question.  The function accepts all key-value tuples. 

\itemize{
 \item Validation: A list controlling the validation. Use \code{\link{construct_validation()}} to construct. 
 \item DisplayLogic : A list controlling the display logic. Use \code{\link{construct_displaylogic()}} to construct.
 \item DataExportTag : A string without whitespace.
 \item QuestionJS : A string with JavaScript code attached to the question. 
 \item Selector and SubSelector: 
  \tabular{lll}{
   Selector \tab SubSelector \tab Choice display \cr
   MAVR \tab 'TX' \tab Multiple choice \cr
   SAVR \tab 'TX' \tab Single choice \cr
   DL \tab \tab Dropdown \cr
   SB \tab \tab Select box \cr
   MSB \tab \tab Multi select box \cr
  }
}
}
\examples{
data(svy_qsf)

# Change data export tag 
qsf <- update_question(qsf, 
	id_name='SecondaryAttribute', 
	id='sex', 
	DataExportTag='gender')

# Convert to multiple choice 
qsf <- update_question(qsf, 
	id_name='SecondaryAttribute', 
	id='sex', 
	Selector='MAVR',
	SubSelector='TX')

# Forced choice for both questions 
qsf_mod <- update_question(svy_qsf, 
 id_name='SecondaryAttribute', 
 id=c('sex', 'born'), 
 Validation=construct_validation(type='force'))





}
