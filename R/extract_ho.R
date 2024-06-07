#' Extract a 'history of' type variable
#'
#' @description
#' Query an RSQLite database and return a data frame with a 0/1 vector depending on whether each individual has at least one observation with relevant code between
#' a specified time period.
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
#' @param numobs Number of obesrvations required to return a value of 1.
#' @param db.open An open SQLite database connection created using RSQLite::dbConnect, to be queried.
#' @param db Name of SQLITE database on hard disk (stored in "data/sql/"), to be queried.
#' @param db.filepath Full filepath to SQLITE database on hard disk, to be queried.
#' @param tab Table name to query in SQLite database.
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
#' This directory structure must be created in advance. `out.subdir` can be used to specify subdirectories within "data/extraction/". These options will use a default naming convention. This can be overwritten
#' using `out.filepath` to manually specify the location on the hard disk to save. Alternatively, return the data frame into the R workspace using `return.output = TRUE`
#' and then save onto the hard disk manually.
#'
#' Codelists can be specified in two ways. The first is to read the codelist into R as a character vector and then specify through the argument
#' `codelist.vector`. Codelists stored on the hard disk can also be referred to from the `codelist` argument, but require a specific underlying directory structure.
#' The codelist on the hard disk must be stored in a directory called "codelists/analysis/" relative to the working directory. The codelist must be a .csv file, and
#' contain a column "medcodeid", "prodcodeid" or "ICD10" depending on the input for argument `tab`. The input to argument `codelist` should just be a character string of
#' the name of the files (excluding the suffix '.csv'). The `codelist.vector` option will take precedence over the `codelist` argument if both are specified.
#'
#' @returns A data frame with a 0/1 vector and patid.
#'
#' @export
extract_ho <- function(cohort,
                       varname = NULL,
                       codelist = NULL,
                       codelist.vector = NULL,
                       indexdt,
                       t = NULL,
                       t.varname = TRUE,
                       time.prev = Inf,
                       time.post = 0,
                       numobs = 1,
                       db.open = NULL,
                       db = NULL,
                       db.filepath = NULL,
                       tab = c("observation", "drugissue", "hes_primary", "death"),
                       out.save.disk = FALSE,
                       out.subdir = NULL,
                       out.filepath = NULL,
                       return.output = TRUE){

  #       varname = "ho_ra_test10"
  #       codelist = "edh_ra_medcodeid"
  #       cohort = cohortA
  #       indexdt = "fup_start"
  #       t = NULL
  #       db = "aurum_nosubset_randset"
  #       db.filepath = NULL
  #       out.save.disk = TRUE
  #       out.filepath = NULL
  #       out.subdir = NULL
  #       return.output = TRUE

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "ho"
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
                     tab = tab,
                     codelist.vector)

  ### Identify which individuals have a history of XXX
  cohort[,"ho"] <- combine_query_boolean(cohort,
                                         db.query = db.qry,
                                         query.type = tab,
                                         time.prev = time.prev,
                                         time.post = time.post,
                                         numobs = numobs)

  ### Reduce to variables of interest
  variable.dat <- cohort[,c("patid", "ho")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "ho"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
