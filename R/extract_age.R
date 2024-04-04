#' Extract age relative to an index date
#'
#' @description
#' Extract age relative to an index date
#'
#' @param cohort Cohort to extract age for.
#' @param varname Optional name for variable in output dataset.
#' @param indexdt Name of variable which defines index date in `cohort`.
#' @param t Number of days after index date at which to calculate variable.
#' @param t.varname Whether to add `t` to `varname`.
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
#' @returns A data frame with variable age
#'
#' @export
extract_age <- function(cohort,
                        varname = NULL,
                        indexdt,
                        t = NULL,
                        t.varname = TRUE,
                        out.save.disk = FALSE,
                        out.subdir = NULL,
                        out.filepath = NULL,
                        return.output = FALSE){

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
  ## Add index date variable to cohort and change indexdt based on t (note the prep_cohort function also removes all other variables)
  cohort <- prep_cohort(cohort, indexdt, t, reduce = FALSE)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "age"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Calculate age
  cohort <- dplyr::mutate(cohort, age = as.numeric((indexdt - dob)/365.25))

  ### Reduce to variables of interest
  variable.dat <- cohort[,c("patid", "age")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "age"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
