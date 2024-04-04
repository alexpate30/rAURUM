#' Extract smoking status prior to index date.
#'
#' @description
#' Extract smoking status prior to index date.
#'
#' @param cohort Cohort to extract age for.
#' @param varname Optional name for variable in output dataset.
#' @param codelist.non Name of codelist (stored on hard disk in "codelists/analysis/") for non-smoker to query the database with.
#' @param codelist.ex Name of codelist (stored on hard disk in "codelists/analysis/") for ex-smoker to query the database with.
#' @param codelist.light Name of codelist (stored on hard disk in "codelists/analysis/") for light smoker to query the database with.
#' @param codelist.mod Name of codelist (stored on hard disk in "codelists/analysis/") for moderate smoker to query the database with.
#' @param codelist.heavy Name of codelist (stored on hard disk in "codelists/analysis/") for heavy smoker to query the database with.
#' @param codelist.non.vector Vector of codes for non-smoker to query the database with.
#' @param codelist.ex.vector Vector of codes for ex-smoker to query the database with.
#' @param codelist.light.vector Vector of codes for light smoker to query the database with.
#' @param codelist.mod.vector Vector of codes for moderate smoker to query the database with.
#' @param codelist.heavy.vector Vector of codes for heavy smoker to query the database with.
#' @param indexdt Name of variable which defines index date in `cohort`.
#' @param t Number of days after index date at which to calculate variable.
#' @param t.varname Whether to add `t` to `varname`.
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
#' We take the most recent smoking status record. If an individuals most recent smoking status is a non-smoker,
#' but they have a history of smoking prior to this, these individuals will be classed as ex-smokers.
#'
#' @returns A data frame with variable smoking status.
#'
#' @export
extract_smoking <- function(cohort,
                            varname = NULL,
                            codelist.non,
                            codelist.ex,
                            codelist.light,
                            codelist.mod,
                            codelist.heavy,
                            codelist.non.vector,
                            codelist.ex.vector,
                            codelist.light.vector,
                            codelist.mod.vector,
                            codelist.heavy.vector,
                            indexdt,
                            t = NULL,
                            t.varname = TRUE,
                            db.open = NULL,
                            db = NULL,
                            db.filepath = NULL,
                            out.save.disk = TRUE,
                            out.subdir = NULL,
                            out.filepath = NULL,
                            return.output = FALSE){

  #   varname = NULL
  #   codelist.non = "edh_smoking_non_medcodeid"
  #   codelist.ex = "edh_smoking_ex_medcodeid"
  #   codelist.light = "edh_smoking_light_medcodeid"
  #   codelist.mod = "edh_smoking_mod_medcodeid"
  #   codelist.heavy = "edh_smoking_heavy_medcodeid"
  #   cohort = cohortZ
  #   indexdt = "fup_start"
  #   t = NULL
  #   db = "aurum_small"
  #   db.filepath = NULL
  #   out.save.disk = TRUE
  #   out.filepath = NULL
  #   out.subdir = NULL
  #   return.output = TRUE

  ### Preparation
  ## Add index date variable to cohort and change indexdt based on t
  cohort <- prep_cohort(cohort, indexdt, t)
  ## Assign variable name if unspecified
  if (is.null(varname)){
    varname <- "smoking"
  }
  ## Change variable name based off time point specified for extraction
  varname <- prep_varname(varname, t, t.varname)
  ## Create named subdirectory if it doesn't exist
  prep_subdir(out.subdir)

  ### Run a database query for type 1 and type 2
  db.qry.non <- db_query(codelist.non,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.non.vector)

  db.qry.ex <- db_query(codelist.ex,
                        db.open = db.open,
                        db = db,
                        db.filepath = db.filepath,
                        tab = "observation",
                        codelist.vector = codelist.ex.vector)

  db.qry.light <- db_query(codelist.light,
                           db.open = db.open,
                           db = db,
                           db.filepath = db.filepath,
                           tab = "observation",
                           codelist.vector = codelist.light.vector)

  db.qry.mod <- db_query(codelist.mod,
                         db.open = db.open,
                         db = db,
                         db.filepath = db.filepath,
                         tab = "observation",
                         codelist.vector = codelist.mod.vector)

  db.qry.heavy <- db_query(codelist.heavy,
                           db.open = db.open,
                           db = db,
                           db.filepath = db.filepath,
                           tab = "observation",
                           codelist.vector = codelist.heavy.vector)

  ### Combine queries with cohort, retaining all smoking records prior to the index date
  ### We treat this as test data, because smoking status may be identified through number of cigarettes smoked per day
  ### We specify value.na.rm = FALSE, as we want to keep the NA values, because smoking status can also be identified through
  ### the medcodeid itself.
  smoking.non <- combine_query(cohort,
                               db.qry.non,
                               query.type = "test",
                               numobs = 100,
                               value.na.rm = FALSE)

  smoking.ex <- combine_query(cohort,
                              db.qry.ex,
                              query.type = "test",
                              numobs = 100,
                              value.na.rm = FALSE)

  smoking.light <- combine_query(cohort,
                                 db.qry.light,
                                 query.type = "test",
                                 numobs = 100,
                                 value.na.rm = FALSE)

  smoking.mod <- combine_query(cohort,
                               db.qry.mod,
                               query.type = "test",
                               numobs = 100,
                               value.na.rm = FALSE)

  smoking.heavy <- combine_query(cohort,
                                 db.qry.heavy,
                                 query.type = "test",
                                 numobs = 100,
                                 value.na.rm = FALSE)

  ### Currently heavy and moderate have no number of cigarettes smoked per day data
  ### Light smoker has lots of data on this, as the codes actually include "light or not stated"
  ### Ex smoker contains lots of values, but cannot be used as looks like they are either used to state
  ### how many a day used to be smoked, or the year at which smoking was given up.
  ### Non-smoker currently has no values bigger than zero
  #   sum(!is.na(smoking.heavy$value))
  #   sum(!is.na(smoking.mod$value))
  #   sum(!is.na(smoking.light$value))
  #   sum(!is.na(smoking.non$value) & smoking.non$value > 0)
  #   sum(!is.na(smoking.ex$value) & smoking.ex$value > 0)

  ### Add the smoking variable to each dataset
  ### Assign a smoking value (0 = non, 1 = ex, 2 = light, 3 = moderate, 4 = heavy) to every observation in each query,
  ### defined solely by the code lists and medical codes
  smoking.non$smoking <- 0
  smoking.ex$smoking <- 1
  smoking.light$smoking <- 2
  smoking.mod$smoking <- 3
  smoking.heavy$smoking <- 4

  ### Change smoking depending on the test value for smoking.light, smoking.mod and smoking.heavy
  ### We set to NA if more than 100
  smoking.light <- dplyr::mutate(smoking.light,
                                 smoking = dplyr::case_when(is.na(value) ~ smoking,
                                                            value == 0 ~ 0,
                                                            value > 0 & value < 10 ~ 2,
                                                            value >= 10 & value < 20 ~ 3,
                                                            value >= 20 & value <= 100 ~ 4,
                                                            value > 100 ~ NA)
  )

  smoking.mod <- dplyr::mutate(smoking.mod,
                               smoking = dplyr::case_when(is.na(value) ~ smoking,
                                                          value == 0 ~ 0,
                                                          value > 0 & value < 10 ~ 2,
                                                          value >= 10 & value < 20 ~ 3,
                                                          value >= 20 & value <= 100 ~ 4,
                                                          value > 100 ~ NA)
  )

  smoking.heavy <- dplyr::mutate(smoking.heavy,
                                 smoking = dplyr::case_when(is.na(value) ~ smoking,
                                                            value == 0 ~ 0,
                                                            value > 0 & value < 10 ~ 2,
                                                            value >= 10 & value < 20 ~ 3,
                                                            value >= 20 & value <= 100 ~ 4,
                                                            value > 100 ~ NA)
  )

  ### Remove the NA values
  smoking.light <- smoking.light[!is.na(smoking)]
  smoking.mod <- smoking.mod[!is.na(smoking)]
  smoking.heavy <- smoking.heavy[!is.na(smoking)]

  ### Only retain the most recent observation for each
  smoking.non <- smoking.non |>
    dplyr::group_by(patid) |>
    dplyr::filter(dplyr::row_number(dplyr::desc(obsdate)) == 1)

  smoking.ex <- smoking.ex |>
    dplyr::group_by(patid) |>
    dplyr::filter(dplyr::row_number(dplyr::desc(obsdate)) == 1)

  smoking.light <- smoking.light |>
    dplyr::group_by(patid) |>
    dplyr::filter(dplyr::row_number(dplyr::desc(obsdate)) == 1)

  smoking.mod <- smoking.mod |>
    dplyr::group_by(patid) |>
    dplyr::filter(dplyr::row_number(dplyr::desc(obsdate)) == 1)

  smoking.heavy <- smoking.heavy |>
    dplyr::group_by(patid) |>
    dplyr::filter(dplyr::row_number(dplyr::desc(obsdate)) == 1)

  ### Concatenate
  variable.dat <- rbind(smoking.non, smoking.ex, smoking.light, smoking.mod, smoking.heavy)

  ### Arrange so that the first observation is the most recent
  ### If there are multiple on the same day, we take the most severe smoking status
  variable.dat <-variable.dat |>
    dplyr::arrange(patid, dplyr::desc(obsdate), dplyr::desc(smoking)) |>
    dplyr::group_by(patid)

  ### Identify those with a smoking history. Given we have only retained one observations from each category,
  ### this means this individual with > 1 observation must have some sort of smoking history.
  smoking.history <- variable.dat |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::filter(count > 1) |>
    dplyr::select(patid) |>
    dplyr::mutate(smoking.history = 1)

  ### Reduce variable.dat to most recent observation only
  variable.dat <- dplyr::slice(variable.dat, 1)

  ### If their most recent value is non-smoker and they have a smoking history, we must change non-smoker to ex-smoker.
  variable.dat <- merge(variable.dat, smoking.history, all.x = TRUE)
  variable.dat <- dplyr::mutate(variable.dat,
                                smoking = dplyr::case_when(smoking == 0 & !is.na(smoking.history) ~ 1,
                                                           TRUE ~ smoking))
  ### Turn into factor variable
  variable.dat$smoking <- factor(variable.dat$smoking,
                                 levels = c(0,1,2,3,4),
                                 labels = c("Non-smoker", "Ex-smoker", "Light", "Moderate", "Heavy"))

  ### Create dataframe of cohort and the variable of interest
  variable.dat <- merge(dplyr::select(cohort, patid), variable.dat, by.x = "patid", by.y = "patid", all.x = TRUE)

  ### Reduce to variables of interest
  variable.dat <- variable.dat[,c("patid", "smoking")]

  ### Change name of variable to varname
  colnames(variable.dat)[colnames(variable.dat) == "smoking"] <- varname

  ### Implement output
  implement_output(variable.dat, varname, out.save.disk, out.subdir, out.filepath, return.output)

}
