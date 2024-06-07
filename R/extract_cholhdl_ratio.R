#' Extract most recent total cholesterol/high-density lipoprotein ratio score relative to an index date.
#'
#' @description
#' Extract most recent total cholesterol/high-density lipoprotein ratio score relative to an index date.
#'
#' @param cohort Cohort to extract age for.
#' @param varname Optional name for variable in output dataset.
#' @param codelist.ratio Name of codelist (stored on hard disk in "codelists/analysis/") for ratio to query the database with.
#' @param codelist.chol Name of codelist (stored on hard disk in "codelists/analysis/") for total cholesterol to query the database with.
#' @param codelist.hdl Name of codelist (stored on hard disk in "codelists/analysis/") for high-density lipoprotein to query the database with.
#' @param codelist.ratio.vector Vector of codes for ratio to query the database with.
#' @param codelist.chol.vector Vector of codes for total cholesterol to query the database with.
#' @param codelist.hdl.vector Vector of codes for high-density lipoprotein to query the database with.
#' @param indexdt Name of variable which defines index date in `cohort`.
#' @param t Number of days after index date at which to calculate variable.
#' @param t.varname Whether to add `t` to `varname`.
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
#' Specifying the non-vector type codelists requires a specific underlying directory structure. The codelist on the hard disk must be stored in "codelists/analysis/" relative
#' to the working directory, must be a .csv file, and contain a column "medcodeid", "prodcodeid" or "ICD10" depending on the chosen `tab`. The input
#' to these variables should just be the name of the files (excluding the suffix .csv). The codelists can also be read in manually, and supplied as a
#' character vector. This option will take precedence over the codelists stored on the hard disk if both are specified.
#'
#' @returns A data frame with variable total cholesterol/high-density lipoprotein ratio.
#'
#' @export
extract_cholhdl_ratio <- function(cohort,
                                  varname = NULL,
                                  codelist.ratio,
                                  codelist.chol,
                                  codelist.hdl,
                                  codelist.ratio.vector,
                                  codelist.chol.vector,
                                  codelist.hdl.vector,
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
                                  return.output = TRUE){

  #           varname = NULL
  #           cohort = cohortZ
  #           codelist.ratio = "edh_cholhdl_ratio_medcodeid"
  #           codelist.chol = "edh_chol_medcodeid"
  #           codelist.hdl = "edh_hdl_medcodeid"
  #           indexdt = "fup_start"
  #           t = 0
  #           t.varname = TRUE
  #           time.prev = round(365.25*5)
  #           time.post = 0
  #           lower.bound = 1
  #           upper.bound = 12
  #           db = "aurum_nosubset_randset"
  #           db.filepath = NULL
  #           out.save.disk = FALSE
  #           out.filepath = NULL
  #           out.subdir = NULL
  #           return.output = TRUE

  ### ADD TEST TO ENSURE THEY SPECIFY TIME FRAME

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "cholhdl_ratio"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Need to run three database queries, one for ratio, one for chol and one for hdl
  db.qry.ratio <- db_query(codelist.ratio,
                           db.open = db.open,
                           db = db,
                           db.filepath = db.filepath,
                           tab = "observation",
                           codelist.vector = codelist.ratio.vector)

  db.qry.chol <- db_query(codelist.chol,
                          db.open = db.open,
                          db = db,
                          db.filepath = db.filepath,
                          tab = "observation",
                          codelist.vector = codelist.chol.vector)

  db.qry.hdl <- db_query(codelist.hdl,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.hdl.vector)

  ### Get latest test result for ratio, chol and hdl from the last five years
  variable.dat.ratio <- combine_query(cohort,
                                      db.qry.ratio,
                                      query.type = "test",
                                      time.prev = time.prev,
                                      time.post = time.post,
                                      lower.bound = lower.bound,
                                      upper.bound = upper.bound)

  variable.dat.chol <- combine_query(cohort,
                                     db.qry.chol,
                                     query.type = "test",
                                     time.prev = time.prev,
                                     time.post = time.post)

  variable.dat.hdl <- combine_query(cohort,
                                    db.qry.hdl,
                                    query.type = "test",
                                    time.prev = time.prev,
                                    time.post = time.post)

  ### Calculate ratio's estimated from chol/hdl
  ## Merge chol and hdl datasets
  variable.dat.manual <- merge(variable.dat.chol, variable.dat.hdl, by.x = "patid", by.y = "patid")
  ## Calculate ratio
  variable.dat.manual$value <- variable.dat.manual$value.x/variable.dat.manual$value.y
  ## Take furthest away date
  variable.dat.manual$obsdate <- pmin(variable.dat.manual$obsdate.x, variable.dat.manual$obsdate.y)
  ## Remove values outside of range
  ### If values are missing, < lower.bound or > upper.bound then delete
  if (!is.null(lower.bound) & !is.null(upper.bound)){
    variable.dat.manual <- variable.dat.manual[value > lower.bound & value < upper.bound]
  } else if (is.null(lower.bound) & !is.null(upper.bound)){
    variable.dat.manual <- variable.dat.manual[value < upper.bound]
  } else if (!is.null(lower.bound) & is.null(upper.bound)){
    variable.dat.manual <- variable.dat.manual[value > lower.bound]
  }
  variable.dat.manual <- variable.dat.manual[,c("patid", "value", "obsdate")]
  rm(variable.dat.chol, variable.dat.hdl)

  ### Merge the two
  variable.dat <- merge(variable.dat.ratio, variable.dat.manual, by.x = "patid", by.y = "patid", all.x = TRUE, all.y = TRUE)

  ### Take most recent of the two
  variable.dat <- dplyr::mutate(variable.dat, cholhdl_ratio = dplyr::case_when(is.na(value.x) & !is.na(value.y) ~ value.y,
                                                                               !is.na(value.x) & is.na(value.y) ~ value.x,
                                                                               !is.na(value.x) & !is.na(value.y) & obsdate.y > obsdate.x ~ value.y,
                                                                               !is.na(value.x) & !is.na(value.y) & obsdate.y <= obsdate.x ~ value.x))

  ### Create dataframe of cohort and the variable of interest
  variable.dat <- merge(dplyr::select(cohort, patid), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "cholhdl_ratio")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "cholhdl_ratio"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
