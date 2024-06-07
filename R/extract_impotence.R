#' Extract impotence status.
#'
#' @description
#' Extract impotence status. This requires either a medical diagnosis, or `drug.numobs` number of prescriptions in `drug.time.prev` number of days
#' prior to the index date.
#'
#' @param cohort Cohort to extract age for.
#' @param varname Optional name for variable in output dataset.
#' @param codelist.med Name of codelist (stored on hard disk in "codelists/analysis/") for medical diagnoses to query the database with.
#' @param codelist.drug Name of codelist (stored on hard disk in "codelists/analysis/") for prescriptions to query the database with.
#' @param codelist.med.vector Vector of codes for medical diagnoses to query the database with.
#' @param codelist.drug.vector Vector of codes for prescriptions to query the database with.
#' @param indexdt Name of variable which defines index date in `cohort`.
#' @param t Number of days after index date at which to calculate variable.
#' @param t.varname Whether to add `t` to `varname`.
#' @param drug.time.prev Number of days prior to index date in which to look for prescriptions.
#' @param drug.numobs Numbre of prescriptions required in time period to indicate impotence.
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
#' @returns A data frame with variable impotence status.
#'
#' @export
extract_impotence <- function(cohort,
                              varname = NULL,
                              codelist.med,
                              codelist.drug,
                              codelist.med.vector,
                              codelist.drug.vector,
                              indexdt,
                              t = NULL,
                              t.varname = TRUE,
                              drug.time.prev = Inf,
                              drug.numobs = 1,
                              db.open = NULL,
                              db = NULL,
                              db.filepath = NULL,
                              out.save.disk = FALSE,
                              out.subdir = NULL,
                              out.filepath = NULL,
                              return.output = TRUE){

  #         varname = NULL
  #         codelist.type1 = "edh_t1dia_medcodeid"
  #         codelist.type2 = "edh_t2dia_medcodeid"
  #         cohort = cohortZ
  #         indexdt = "fup_start"
  #         t = NULL
  #         db = "aurum_small"
  #         db.filepath = NULL
  #         out.save.disk = TRUE
  #         out.filepath = NULL
  #         out.subdir = NULL
  #         return.output = TRUE

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "impotence"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Run a database query for type 1 and type 2
  db.qry.med <- db_query(codelist.med,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.med.vector)

  db.qry.drug <- db_query(codelist.drug,
                          db.open = db.open,
                          db = db,
                          db.filepath = db.filepath,
                          tab = "observation",
                          codelist.vector = codelist.drug.vector)

  ### Identify which individuals have a history of type 1 or type 2
  cohort[,"med"] <- combine_query_boolean(cohort,
                                          db.qry.med,
                                          query.type = "obs")

  cohort[,"drug"] <-   combine_query_boolean(cohort,
                                             db.query = db.qry.drug,
                                             query.type = "drug",
                                             time.prev = drug.time.prev,
                                             time.post = 0,
                                             numobs = drug.numobs)

  ### Create the varaible of interest from these
  ### if an individual has history of type 1 and type 2, they will be classed as type 1
  variable.dat <- dplyr::mutate(cohort, impotence = pmax(med, drug))

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "impotence")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "impotence"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
