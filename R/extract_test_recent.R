#' Extract test data.
#'
#' @description
#' Query an RSQLite database and return a data frame containing the most recent test result that meets specified criteria.
#'
#' @param cohort Cohort of individuals to extract the 'history of' variable for.
#' @param varname Name of variable in the outputted data frame.
#' @param codelist Name of codelist (stored on hard disk) to query the database with.
#' @param codelist.vector Vector of codes to query the database with. This takes precedent over `codelist` if both are specified.
#' @param indexdt Name of variable in `cohort` which specifies the index date. The extracted variable will be calculated relative to this.
#' @param t Number of days after \code{indexdt} at which to extract the variable.
#' @param t.varname Whether to alter the variable name in the outputted data frame to reflect `t`.
#' @param time.prev Number of days prior to index date to look for codes.
#' @param time.post Number of days after index date to look for codes.
#' @param lower.bound Lower bound for returned values.
#' @param upper.bound Upper bound for returned values.
#' @param db.open An open SQLite database connection created using RSQLite::dbConnect, to be queried.
#' @param db Name of SQLITE database on hard disk (stored in "data/sql/"), to be queried.
#' @param db.filepath Full filepath to SQLITE database on hard disk, to be queried.
#' @param out.save.disk If `TRUE` will attempt to save outputted data frame to directory "data/extraction/".
#' @param out.subdir Sub-directory of "data/extraction/" to save outputted data frame into.
#' @param out.filepath Full filepath and filename to save outputted data frame into.
#' @param return.output If `TRUE` will return outputted data frame into R workspace.
#'
#' @details Specifying `db` requires a specific underlying directory structure. The SQLite database must be stored in "data/sql/" relative to the working directory.
#' If the SQLite database is accessed through `db`, the connection will be opened and then closed after the query is complete. The same is true if
#' the database is accessed through `db.filepath`. A connection to the SQLite database can also be opened manually using `RSQLite::dbConnect`, and then
#' using the object as input to parameter `db.open`. After wards, the connection must be closed manually using `RSQLite::dbDisconnect`. If `db.open` is specified, this will take precedence over `db` or `db.filepath`.
#'
#' If `out.save.disk = TRUE`, the data frame will automatically be written to an .rds file in a subdirectory "data/extraction/" of the working directory.
#' This directory structure must be created in advance. `out.subdir` can be used to specify subdirectories within "data/extraction/". These options will use a default naming convetion. This can be overwritten
#' using `out.filepath` to manually specify the location on the hard disk to save. Alternatively, return the data frame into the R workspace using `return.output = TRUE`
#' and then save onto the hard disk manually.
#'
#' Codelists can be specified in two ways. The first is to read the codelist into R as a character vector and then specify through the argument
#' `codelist.vector`. Codelists stored on the hard disk can also be referred to from the `codelist` argument, but require a specific underlying directory structure.
#' The codelist on the hard disk must be stored in a directory called "codelists/analysis/" relative to the working directory. The codelist must be a .csv file, and
#' contain a column "medcodeid", "prodcodeid" or "ICD10" depending on the input for argument `tab`. The input to argument `codelist` should just be a character string of
#' the name of the files (excluding the suffix '.csv'). The `codelist.vector` option will take precedence over the `codelist` argument if both are specified.
#'
#' Currently only returns most recent test result. This will be updated to return more than one most recent test result if specified.
#'
#' @returns A data frame containing most recent test result that meets required criteria.
#'
#' @examples
#'
#' ## Connect
#' aurum_extract <- connect_database(tempfile("temp.sqlite"))
#'
#' ## Create SQLite database using cprd_extract
#' cprd_extract(aurum_extract,
#' filepath = system.file("aurum_data", package = "rAURUM"),
#' filetype = "observation", use.set = FALSE)
#'
#' ## Define cohort and add index date
#' pat<-extract_cohort(system.file("aurum_data", package = "rAURUM"))
#' pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
#'
#' ## Extract most recent test value prior to index date
#' extract_test_data(pat,
#' codelist.vector = "187341000000114",
#' indexdt = "fup_start",
#' db.open = aurum_extract,
#' time.prev = Inf,
#' return.output = TRUE)
#'
#' @export
extract_test_recent <- function(cohort,
                                varname = NULL,
                                codelist = NULL,
                                codelist.vector = NULL,
                                indexdt,
                                t = NULL,
                                t.varname = TRUE,
                                time.prev = 365.25*5,
                                time.post = 0,
                                lower.bound = -Inf,
                                upper.bound = Inf,
                                db.open = NULL,
                                db = NULL,
                                db.filepath = NULL,
                                out.save.disk = FALSE,
                                out.subdir = NULL,
                                out.filepath = NULL,
                                return.output = FALSE){

  #           varname = NULL
  #           codelist = "edh_sbp_medcodeid"
  #           cohort = cohortZ
  #           indexdt = "fup_start"
  #           t = 0
  #           t.varname = TRUE
  #           time.prev = 365.25*5
  #           time.post = 0
  #           lower.bound = -Inf
  #           upper.bound = Inf
  #           db = "aurum_small"
  #           db.filepath = NULL
  #           out.save.disk = FALSE
  #           out.filepath = NULL
  #           out.subdir = NULL
  #           return.output = TRUE

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "value"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Run a database query
  db.qry <- db_query(codelist,
                     db.open = db.open,
                     db = db,
                     db.filepath = db.filepath,
                     tab = "observation",
                     codelist.vector = codelist.vector)

  ### Get test data for individuals in cohort, within time range and remove outliers
  variable.dat <- combine_query(cohort,
                                db.qry,
                                query.type = "test",
                                time.prev = time.prev,
                                time.post = time.post,
                                lower.bound = lower.bound,
                                upper.bound = upper.bound)

  ### Create dataframe of cohort and the variable of interest
  variable.dat <- merge(dplyr::select(cohort, patid), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "value", "numunitid")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "value"] <- varname

  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
