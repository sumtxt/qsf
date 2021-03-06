% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/append_survey.R
\name{append_survey}
\alias{append_survey}
\title{Append survey questions to an existing survey}
\usage{
append_survey(qsf, sq)
}
\arguments{
\item{qsf}{qsf file as R list}

\item{sq}{list of survey questions (elements)}
}
\description{
Transplanting survey elements (e.g., questions) from one survey to another
}
\details{
Note that new survey questions also need to be attached to a question block (see \code{\link{attach_question}}).
}
\examples{
data(svy_qsf)

# Copy Q1 from survey 
q <- get_question(svy_qsf, id_name='PrimaryAttribute', id='Q1', df=FALSE)

# Remove Q1 from survey 
qsf <- remove_question(svy_qsf, id_name='PrimaryAttribute', id='Q1')

# Reappend Q1 to survey
qsf <- append_survey(qsf, list(q))
 

}
