#' Extract a 'time until' type variable
#'
#' @description
#' Query an RSQLite database and a data frame with the time until first code of interest or censoring, and an event/censoring indicator.
#'
#' @param cohort Cohort of individuals to extract the 'history of' variable for.
#' @param varname.time Name of time variable in the outputted data frame.
#' @param varname.indicator Name of event/censoring indicator in the outputted data frame.
#' @param codelist Name of codelist (stored on hard disk) to query the database with.
#' @param codelist.vector Vector of codes to query the database with. This takes precedent over `codelist` if both are specified.
#' @param indexdt Name of variable in `cohort` which specifies the index date. The extracted variable will be calculated relative to this.
#' @param censdt Name of variable in `cohort` which specifies the censoring date.
#' @param censdt.lag Number of days after censoring where events will still be considered, to account for delays in recording.
#' @param t Number of days after \code{indexdt} at which to extract the variable.
#' @param t.varname Whether to alter the variable name in the outputted data frame to reflect `t`.
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
#' If the time until event is the same as time until censored, this will be considered an event (var_indicator = 1)
#'
#' If `dtcens.lag > 0`, then the time until the event of interest will be the time until the minimum of the event of interest, and date of censoring.
#'
#' @returns A data frame with variable patid, a variable containing the time until event/censoring, and a variable containing event/censoring indicator.
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
#' ## Define cohort and add index date and censoring date
#' pat<-extract_cohort(system.file("aurum_data", package = "rAURUM"))
#' pat$indexdt <- as.Date("01/01/1955", format = "%d/%m/%Y")
#' pat$fup_end <- as.Date("01/01/2000", format = "%d/%m/%Y")
#'
#' ## Extract time until event/censoring
#' extract_time_until(pat,
#' codelist.vector = "187341000000114",
#' indexdt = "fup_start",
#' censdt = "fup_end",
#' db.open = aurum_extract,
#' tab = "observation",
#' return.output = TRUE)
#'
#' @export
extract_time_until <- function(cohort,
                               varname.time = NULL,
                               varname.indicator = NULL,
                               codelist = NULL,
                               codelist.vector = NULL,
                               indexdt,
                               censdt,
                               censdt.lag = 0,
                               t = NULL,
                               t.varname = TRUE,
                               db.open = NULL,
                               db = NULL,
                               db.filepath = NULL,
                               tab = c("observation", "drugissue", "hes_primary", "death"),
                               out.save.disk = FALSE,
                               out.subdir = NULL,
                               out.filepath = NULL,
                               return.output = FALSE){

  #         varname.time = NULL
  #         varname.indicator = NULL
  #         codelist = "edh_cvd_hist_medcodeid"
  #         cohort = cohortZ
  #         indexdt = "fup_start"
  #         t = NULL
  #         db = "aurum_small"
  #         tab = "obs"
  #         db.filepath = NULL
  #         out.save.disk = TRUE
  #         out.filepath = NULL
  #         out.subdir = NULL
  #         return.output = TRUE

  # cohort = pat
  # varname.time = NULL
  # varname.indicator = NULL
  # codelist.vector = codelist
  # indexdt = "fup_start"
  # censdt = "fup_end"
  # db.open = aurum_extract3
  # tab = "observation"
  # return.output = TRUE
  #         db.filepath = NULL
  #         out.save.disk = FALSE
  #         out.filepath = NULL
  #         out.subdir = NULL

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t, reduce = FALSE)
  ### Change name of censoring date variable to "censdt" so we can easily refer to it
  colnames(cohort)[colnames(cohort) == censdt] <- "censdt"

  ## Reduce cohort to variables of interest
  cohort <- cohort[,c("patid", "indexdt", "censdt")]
  ## Assign variable name if unspecified
  if (is.null(varname.time)){
    varname.time <- "var_time"
  }
  if (is.null(varname.indicator)){
    varname.indicator <- "var_indicator"
  }

  ## Change variable name based off time point specified for extraction
  varname.time <- prep_varname(varname.time, t, t.varname)
  varname.indicator <- prep_varname(varname.indicator, t, t.varname)

  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Run a database query
  db.qry <- db_query(codelist,
                     db.open = db.open,
                     db = db,
                     db.filepath = db.filepath,
                     tab = tab,
                     codelist.vector = codelist.vector)

  ### Identify the first CVD event happening after the indexdt
  ## If tab = "observation", this could be a query.type of "med" or "test", choose "med" as not interested in test results themselves
  query.type <- tab
  if (query.type == "observation"){query.type <- "med"}

  ## Combine query
  ## reduce.output = FALSE because we want access to censdt and
  variable.dat <- combine_query(cohort,
                                db.query = db.qry,
                                query.type = query.type,
                                time.prev = 0,
                                time.post = Inf,
                                numobs = 1,
                                value.na.rm = FALSE,
                                earliest.values = TRUE,
                                reduce.output = FALSE)

  ### Calculate the time until event of interest, set to NA and remove if beyond censdt
  variable.dat <-
    dplyr::mutate(variable.dat,
                  var_time = dplyr::case_when(obsdate <= censdt + censdt.lag ~ pmin(obsdate, censdt) - as.numeric(indexdt),
                                              obsdate > censdt + censdt.lag ~ NA),
                  var_indicator = dplyr::case_when(!is.na(var_time) ~ 1,
                                                   TRUE ~ NA)) |>
    dplyr::filter(!is.na(var_time))

  ### Reduce to variables of interst
  variable.dat <- variable.dat[,c("patid", "var_time", "var_indicator")]

  ### Merge back with cohort
  variable.dat <- merge(dplyr::select(cohort, patid, indexdt, censdt), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### If the event has NA, set the time to censdt, and indicator to 0
  variable.dat <- dplyr::mutate(variable.dat,
                                var_indicator = dplyr::case_when(!is.na(var_time) ~ var_indicator,
                                                                 is.na(var_time) ~ 0),
                                var_time = dplyr::case_when(!is.na(var_time) ~ var_time,
                                                            is.na(var_time) ~ as.numeric(censdt - indexdt))
  )

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "var_time", "var_indicator")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "var_time"] <- varname.time
  colnames(variable.dat)[colnames(variable.dat) == "var_indicator"] <- varname.indicator

  ### Implement output
  implement_output(variable.dat, varname.time, out.save.disk, out.subdir, out.filepath, return.output)

}
