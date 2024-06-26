% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{combine_query_boolean}
\alias{combine_query_boolean}
\title{Combine a database query with a cohort returning a 0/1 vector depending on whether each individual has a recorded code of interest.}
\usage{
combine_query_boolean(
  cohort,
  db.query,
  query.type = c("observation", "drugissue", "hes_primary"),
  time.prev = Inf,
  time.post = 0,
  numobs = 1
)
}
\arguments{
\item{cohort}{Cohort to combine with the database query.}

\item{db.query}{Output from database query (ideally obtained through \code{\link{db_query}}).}

\item{query.type}{Type of query}

\item{time.prev}{Number of days prior to index date to look for codes.}

\item{time.post}{Number of days after index date to look for codes.}

\item{numobs}{Number of observations required to be observed in specified time window to return a 1.}
}
\value{
A 0/1 vector.
}
\description{
Combine a database query with a cohort returning a 0/1 vector depending on whether each individual has a recorded code of interest.
\code{cohort} must contain variables \code{patid} and \code{indexdt}. The database query will be merged with the cohort by variable \code{patid}.
If an individual has at least \code{numobs} observations between \code{time.prev} days prior to \code{indexdt}, and \code{time.post} days after
\code{indexdt}, a 1 will be returned, 0 otherwise. The \code{type} of query must be specified for appropriate data manipulation.
}
\examples{
## Create connection to a temporary database
aurum_extract <- connect_database(tempfile("temp.sqlite"))

## Add observation data from all observation files in specified directory
cprd_extract(db = aurum_extract,
filepath = system.file("aurum_data", package = "rAURUM"),
filetype = "observation")

## Query database for a specific medcode
db.query <- db_query(db.open = aurum_extract,
tab ="observation",
codelist.vector = "187341000000114")

## Define cohort
pat<-extract_cohort(filepath = system.file("aurum_data", package = "rAURUM"))

### Add an index date to pat
pat$indexdt <- as.Date("01/01/2020", format = "\%d/\%m/\%Y")

## Combine query with cohort creating a 'history of' boolean variable
combine_query_boolean(cohort = pat,
db.query = db.query,
query.type = "observation")

}
