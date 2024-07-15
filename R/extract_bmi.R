#' Extract most recent BMI score relative to an index date.
#'
#' @description
#' Extract most recent BMI score relative to an index date.
#'
#' @param cohort Cohort to extract age for.
#' @param varname Optional name for variable in output dataset.
#' @param codelist.bmi Name of codelist (stored on hard disk in "codelists/analysis/") for BMI to query the database with.
#' @param codelist.weight Name of codelist (stored on hard disk in "codelists/analysis/") for weight to query the database with.
#' @param codelist.height Name of codelist (stored on hard disk in "codelists/analysis/") for height to query the database with.
#' @param codelist.bmi.vector Vector of codes for BMI to query the database with.
#' @param codelist.weight.vector Vector of codes for weight to query the database with.
#' @param codelist.height.vector Vector of codes for height to query the database with.
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
#' @details BMI can either be identified through a directly recorded BMI score, or calculated via height and weight scores.
#' Full details on the algorithm for extracting BMI are given in the vignette: Details-on-algorithms-for-extracting-specific-variables.
#' This vignette can be viewed by running \code{vignette("help", package = "rAURUM)}.
#'
#' Specifying `db` requires a specific underlying directory structure. The SQLite database must be stored in "data/sql/" relative to the working directory.
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
#' @returns A data frame with variable BMI.
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
#' ## Extract most recent BMI prior to index date
#' extract_bmi(cohort = pat,
#' codelist.bmi.vector = "498521000006119",
#' codelist.weight.vector = "401539014",
#' codelist.height.vector = "13483031000006114",
#' indexdt = "indexdt",
#' time.prev = Inf,
#' db.open = aurum_extract,
#' return.output = TRUE)
#'
#' @export
extract_bmi <- function(cohort,
                        varname = NULL,
                        codelist.bmi,
                        codelist.weight,
                        codelist.height,
                        codelist.bmi.vector,
                        codelist.weight.vector,
                        codelist.height.vector,
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

  #         varname = NULL
  #         cohort = cohortZ
  #         codelist.bmi = "edh_bmi_medcodeid"
  #         codelist.height = "height_medcodeid"
  #         codelist.weight = "weight_medcodeid"
  #         indexdt = "fup_start"
  #         t = 0
  #         t.varname = TRUE
  #         time.prev = round(365.25*5)
  #         time.post = 0
  #         lower.bound = 18
  #         upper.bound = 47
  #         db = "aurum_small"
  #         db.filepath = NULL
  #         out.save.disk = FALSE
  #         out.filepath = NULL
  #         out.subdir = NULL
  #         return.output = TRUE

  ### ADD TEST TO ENSURE THEY SPECIFY TIME FRAME

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "bmi"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Need to run three database queries, one for BMI, one for height and one for weight
  ### BMI
  db.qry.bmi <- db_query(codelist.bmi,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.bmi.vector)

  variable.dat.bmi <- combine_query(cohort,
                                    db.qry.bmi,
                                    query.type = "test",
                                    time.prev = time.prev,
                                    time.post = time.post,
                                    lower.bound = lower.bound,
                                    upper.bound = upper.bound)

  ### Height
  db.qry.height <- db_query(codelist.height,
                            db.open = db.open,
                            db = db,
                            db.filepath = db.filepath,
                            tab = "observation",
                            codelist.vector = codelist.height.vector)

  variable.dat.height <- combine_query(cohort,
                                       db.qry.height,
                                       query.type = "test",
                                       time.prev = time.prev,
                                       time.post = time.post)

  ### Weight
  db.qry.weight <- db_query(codelist.weight,
                            db.open = db.open,
                            db = db,
                            db.filepath = db.filepath,
                            tab = "observation",
                            codelist.vector = codelist.weight.vector)

  variable.dat.weight <- combine_query(cohort,
                                       db.qry.weight,
                                       query.type = "test",
                                       time.prev = time.prev,
                                       time.post = time.post)

  ### For the height query, we need to rescale those with numunitid = 173, 432 or 3202 from metres to cm
  db.qry.height <- dplyr::mutate(db.qry.height, value = dplyr::case_when(numunitid %in% c(173, 432, 3202) ~ value,
                                                                         !(numunitid %in% c(173, 432, 3202)) ~ value/100))





  ### Calculate bmi's estimated from height/weight
  ## Merge height and weight datasets
  variable.dat.manual <- merge(variable.dat.weight, variable.dat.height, by.x = "patid", by.y = "patid")
  ## Calculate bmi
  variable.dat.manual$value <- variable.dat.manual$value.x/(variable.dat.manual$value.y)^2
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
  rm(variable.dat.height, variable.dat.weight)

  ### Merge the two
  variable.dat <- merge(variable.dat.bmi, variable.dat.manual, by.x = "patid", by.y = "patid", all.x = TRUE, all.y = TRUE)

  ### Take most recent of the two
  variable.dat <- dplyr::mutate(variable.dat, bmi = dplyr::case_when(is.na(value.x) & !is.na(value.y) ~ value.y,
                                                                     !is.na(value.x) & is.na(value.y) ~ value.x,
                                                                     !is.na(value.x) & !is.na(value.y) & obsdate.y > obsdate.x ~ value.y,
                                                                     !is.na(value.x) & !is.na(value.y) & obsdate.y <= obsdate.x ~ value.x))

  ### Create dataframe of cohort and the variable of interest
  variable.dat <- merge(dplyr::select(cohort, patid), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "bmi")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "bmi"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
