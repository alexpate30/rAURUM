% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{combine_query}
\alias{combine_query}
\title{Combine a database query with a cohort.}
\usage{
combine_query(
  cohort,
  db.query,
  query.type = c("med", "test", "drugissue", "hes_primary", "death"),
  time.prev = Inf,
  time.post = Inf,
  lower.bound = -Inf,
  upper.bound = Inf,
  numobs = 1,
  value.na.rm = TRUE,
  earliest.values = FALSE,
  reduce.output = TRUE
)
}
\arguments{
\item{cohort}{Cohort to combine with the database query.}

\item{db.query}{Output from database query (ideally obtained through \code{\link{db_query}}).}

\item{query.type}{Type of query}

\item{time.prev}{Number of days prior to index date to look for codes.}

\item{time.post}{Number of days after index date to look for codes.}

\item{lower.bound}{Lower bound for returned values when \code{query.type = "test"}.}

\item{upper.bound}{Upper bound for returned values when \code{query.type = "test"}.}

\item{numobs}{Number of observations to be returned.}

\item{value.na.rm}{If TRUE will remove data with NA in the \code{value} column of the queried data and remove values outside of \code{lower.bound} and \code{upper.bound} when \code{query.type = "test"}.}

\item{earliest.values}{If TRUE will return the earliest values as opposed to most recent.}

\item{reduce.output}{If TRUE will reduce output to just \code{patid}, \code{obsdate}, medical/product code, and test \code{value}.}
}
\value{
A data.table with observations that meet specified criteria.
}
\description{
Combine a database query with a cohort, only retaining observations between \code{time.prev} days prior to \code{indexdt}, and \code{time.post} days after
\code{indexdt}, and for test data with values between \code{lower.bound} and \code{upper.bound}. The most recent \code{numobs} observations will be returned.
\code{cohort} must contain variables \code{patid} and \code{indexdt}. The \code{type} of query must be specified for appropriate data manipulation. Input \code{type = med} if
interested in medical diagnoses from the observation file, and \code{type = test} if interseted in test data from the observation file.
}
\details{
\code{value.na.rm = FALSE} may be of use when extracting variables like smoking status, where we want test data for number of cigarettes per day,
but do not want to remove all observations with NA in the \code{value} column, because the medcodeid itself may indicate smoking status.
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

## Combine query with cohort retaining most recent three records
combine_query(cohort = pat,
db.query = db.query,
query.type = "med",
numobs = 3)

}
